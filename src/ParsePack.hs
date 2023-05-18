{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE DataKinds         #-}

module ParsePack (buildReader, IData(..), IdxFile(..), Object(..)) where

import Delta ( patch )
import Prelude                                      hiding (take, either)
import Conduit
    ( MonadIO(..),
      MonadTrans(lift),
      (.|),
      await,
      leftover,
      yield,
      runReaderC,
      ConduitT,
      Flush(..),
      MonadThrow(throwM),
      PrimMonad,
      MonadResource,
      ResourceT )
import Data.Conduit.Combinators ( fold, headE, take )
import Data.Bifunctor                                      (bimap)
import Data.Word                                           (Word32, Word8)
import Data.Ord                                            (comparing)
import Data.Foldable                                       (foldl')
import Data.ByteString                                     (ByteString)
import Data.Bits                                           (shiftL, Bits ((.|.), (.&.)), shiftR)
import Data.IORef ( IORef, modifyIORef', newIORef, readIORef )
import Data.Vector.Mutable                                 (IOVector)
import Data.Streaming.Zlib
    ( feedInflate,
      flushInflate,
      getUnusedInflate,
      initInflate,
      defaultWindowBits,
      PopperRes(PRError, PRDone, PRNext),
      WindowBits )
import Data.ByteArray                                      (convert)
import Data.ByteString.Base16                              (encode)
-- import Data.List.NonEmpty                                  (NonEmpty)
import Crypto.Hash ( hashWith, SHA1(SHA1) )
import Control.Exception ( Exception )
import Control.Monad.Reader
    ( replicateM, replicateM_, unless, ReaderT, fix, asks )
import Control.Monad.Primitive                             (unsafePrimToPrim)
import Data.Digest.CRC32 ( CRC32(crc32) )
-- import qualified Data.ByteString.Lazy  as L
import qualified Prelude                             as P  (either)
import qualified Data.ByteString.Char8               as C
import qualified Data.ByteString                     as B
import qualified Data.Vector.Algorithms.Merge        as MV
-- import qualified Data.Vector.Algorithms.Search       as VS
-- import qualified Data.Vector.Algorithms.AmericanFlag as AV
import qualified Data.IntMap.Strict                  as I
import qualified Data.Vector.Mutable                 as MU
import qualified Data.Vector.Unboxed.Sized           as UV
import qualified Data.Vector                         as V
import System.PosixCompat.Files ()

-- import qualified Data.List.NonEmpty                  as N

data ParsePackException = SignatureException
                        | VersionException
                        | UnexpectedEndOfInput
                        | BadObject
                        | PatchingError
                        | MissingBase
                        | ErrorBuildingWorkingDirectory
                        | ErrorParsingPackedObject

instance Show ParsePackException where
  show SignatureException            = "Wrong Packfile signature"
  show VersionException              = "Wrong Packfile version, expected 2"
  show UnexpectedEndOfInput          = "Premature end of stream"
  show BadObject                     = "Corrupted object"
  show PatchingError                 = "Error patching delta"
  show MissingBase                   = "Base Object missing"
  show ErrorBuildingWorkingDirectory = "Error building working directory"
  show ErrorParsingPackedObject      = "Error parsing packed object"

instance Exception ParsePackException

type R      = ResourceT IO
type M env  = ReaderT env R
type Offset = Word32
type CheckS = Word32


data IData = IData {
      sha :: ByteString
    , chc :: CheckS
    , off :: Offset
    }

instance Show IData where
  show IData{..} = show (encode sha) <> " " <> show chc <> " " <> show off

data PackType = BLOB
              | TREE
              | COMMIT
              | TAG
              | OFS_DELTA Int
              | OFS_REF   [Word8]
              | BAD
              | NONE
              | ANY
              | MAX
              deriving (Show, Eq)

data Object = Object {
    gsize :: Int
  , gtype :: GitType
  , gload :: ByteString
  } deriving (Eq, Show)

data GitType = Commit | Tree | Tag | Blob
  deriving (Eq, Show)

showB :: GitType -> ByteString
showB Commit = "commit"
showB Tree   = "tree"
showB Tag    = "tag"
showB Blob   = "blob"

toGitType :: PackType -> GitType
toGitType BLOB   = Blob
toGitType TREE   = Tree
toGitType COMMIT = Commit
toGitType TAG    = Tag
toGitType _      = error "Problem"

data IdxFile = IdxFile {
    sign :: Word32
  , vers :: Word32
  , fout :: UV.Vector 256 Word32
  , pyld :: V.Vector IData
  , psha :: ByteString
  }

header :: ConduitT ByteString o (ResourceT IO) Int
header = do
  x <- (,,) <$> (w8sTow32 <$> getBytes 4) <*> (w8sTow32 <$> getBytes 4) <*> (w8sTow32 <$> getBytes 4)
  case x of
    (0x5041434b,2,n) -> return $ fromIntegral n
    (_         ,2,_) -> liftIO $ throwM SignatureException
    _                -> liftIO $ throwM VersionException

createEnv :: MonadIO m => Int -> m Env
createEnv n =
      Env
  <$> liftIO (newIORef 12)
  <*> liftIO (newIORef mempty)
  <*> liftIO (MU.replicate n $ IData { sha = mempty, chc = 0, off = 0 })
  <*> liftIO (newIORef 0)
  <*> liftIO (newIORef mempty)

data Env = Env {
    index :: IORef Int
  , bases :: IORef (I.IntMap Object)
  , idata :: MU.IOVector  IData
  , obidx :: IORef Int
  , pdata :: IORef ByteString
  }

buildReader :: ConduitT ByteString o R (I.IntMap Object, IdxFile)
buildReader = header >>= parsePackFile

parsePackFile :: Int -> ConduitT ByteString o (ResourceT IO) (I.IntMap Object, IdxFile)
parsePackFile n = do
  env <- liftIO $ createEnv n
  runReaderC env $ do
    parseObjects n
    psha     <- parsePackfileSha .| fold
    shas     <- asks idata
    MV.sortBy (comparing sha)         shas
    fanout   <- liftIO $ buildFanout  shas
    hashes   <- V.freeze shas
    objects  <- getIOVar bases
    let idxFile = IdxFile { --name = filename
                            sign = 0xff744f63
                          , vers = 2
                          , fout = fanout
                          , pyld = hashes
                          , psha = psha
                          }
    return (objects, idxFile)

parsePackfileSha :: ConduitT ByteString ByteString (M Env) ()
parsePackfileSha = take 20

buildFanout :: IOVector IData -> IO (UV.Vector 256 Word32)
buildFanout v = do
    let fanout = UV.replicate 0
    MU.foldl' f fanout v
  where
    f :: UV.Vector 256 Word32 -> IData -> UV.Vector 256 Word32
    f uv IData{..} =
      let h      = fromIntegral $ B.head sha
          fromTo = (, 1) <$>  [h .. 255]
       in UV.accum (+) uv fromTo

parseObjects :: Int -> ConduitT ByteString o (M Env) ()
parseObjects n = replicateM_ n (parseObject >> modIOVar obidx succ)

parseObject :: ConduitT ByteString o (M Env) ()
parseObject = do
   offset         <- getIOVar index
   (size', type') <- parseHeader
   dbytes         <- decompress defaultWindowBits .| fold
   case type' of
     OFS_DELTA n -> do
        mo <- I.lookup (offset - n) <$>  getIOVar bases
        case mo of
          Nothing         -> throwM MissingBase
          Just Object{..} -> do
            let datum = patch gload dbytes
            either datum (throwM . const PatchingError) $ \d -> do
              let len  = B.length d
                  sha1 = computeHash gtype len d
              modIOVar bases (I.insert offset (Object len gtype d))
              oidx <- getIOVar obidx
              shas <- asks idata
              cr32 <- crc32 <$> getIOVar pdata
              liftIO $ MU.write shas oidx $ IData sha1 cr32 (fromIntegral offset)

           --  1) look for base  
           --  2) patch 
           --  3) insert in base 
           --  4) compute Hash 
     OFS_REF w8s -> undefined -- TODO #if it's in the pack search in base and do the same as above 
     _           -> do
        let trueType = toGitType type'
            sha1 = computeHash trueType size' dbytes
        modIOVar bases (I.insert offset (Object size' trueType dbytes))
        oidx <- getIOVar obidx
        shas <- asks idata
        cr32 <- crc32 <$> getIOVar pdata
        liftIO $ MU.write shas oidx $ IData sha1 cr32 (fromIntegral offset)
   modIOVar pdata (const mempty)

computeHash :: GitType -> Int -> ByteString -> ByteString
computeHash type' size' data' =
  let buildHeader = showB type' <> " " <> C.pack (show size') <> "\0"
    in convert $ hashWith SHA1 $ buildHeader <> data'

parseHeader :: ConduitT ByteString o (M Env) (Int,PackType)
parseHeader = do
  w8 <- getByte
  modIOVar pdata (`B.snoc` w8)
  modIOVar index (1+)
  let ntype = w8 `shiftR` 4 .&. 7
      begin = fromIntegral $ w8 .&. 15
  size' <- if msb w8 then parseObjectSize begin 0 else return begin
  type' <- toPackType ntype
  return (size', type')

parseObjectSize :: Int -> Int -> ConduitT ByteString o (M Env) Int
parseObjectSize  size' iter =  do
  w8 <- getByte
  modIOVar pdata (`B.snoc` w8)
  let add = (fromIntegral ( w8 .&. 127) :: Int) `shiftL` (4 + iter * 7)
      acc = size' + add
  if msb w8 then parseObjectSize acc (iter + 1) else modIOVar index (+ (iter + 1)) >> return acc

toPackType :: Word8 -> ConduitT ByteString o (M Env) PackType
toPackType 1 = return COMMIT
toPackType 2 = return TREE
toPackType 3 = return BLOB
toPackType 4 = return TAG
toPackType 6 = do
  (l,ws) <- parseOffset
  modIOVar index (l +)
  return $ OFS_DELTA $ foldOff l ws
toPackType 7 =  modIOVar index (20 +) >> OFS_REF <$> getBytes 20 -- TODO add to non list 
toPackType _ = liftIO $ throwM BadObject

parseOffset :: ConduitT ByteString o (M Env ) (Int,[Word8])
parseOffset = do
  w8 <- getByte
  modIOVar pdata (`B.snoc` w8)
  if msb w8
    then bimap (1+) ((w8 .&. 127):) <$> parseOffset
    else return (1,[w8])

foldOff :: Int -> [Word8] -> Int
foldOff len w8s =
  let toAdd    = if len < 2 then 0 else sum $ Prelude.zipWith (\x _ -> 2 ^x) [7 :: Int,14 ..] [1 .. len - 1]
      f acc w8 = acc `shiftL` 7 + fromIntegral w8
    in toAdd + foldl' f 0 w8s

decompress ::  WindowBits -> ConduitT ByteString ByteString (M Env) ()

decompress = helpDecompress (fmap (fmap Chunk) await) yield' leftover
  where
    yield' Flush      = return ()
    yield' (Chunk bs) = yield bs

helpDecompress :: ConduitT ByteString ByteString (M Env) (Maybe (Flush ByteString))
               -> (Flush ByteString -> ConduitT ByteString ByteString (M Env) ())
               -> (ByteString -> ConduitT ByteString ByteString (M Env) ())
               -> WindowBits
               -> ConduitT ByteString ByteString (M Env) ()
helpDecompress  await' yield' leftover' config = do
  inf <- lift $ unsafeLiftIO $ initInflate config

  let flush = do
        chunk <- lift $ unsafeLiftIO $ flushInflate inf
        unless (B.null chunk) $ yield' $ Chunk chunk

      getUnused = lift $ unsafeLiftIO $ getUnusedInflate inf

      unused = do
        rem' <- getUnused
        unless (B.null rem') $ do
                let len = B.length rem'
                modIOVar index (\i -> i - len)
                modIOVar pdata (B.dropEnd  len)
                leftover' rem'

  fix $ \feeder -> do
    mnext <- await'
    case mnext of
      Nothing -> do
        flush
        unused
      Just (Chunk x) -> do
        modIOVar index (B.length x +)
        modIOVar pdata (<> x)
        popper <- lift $ unsafeLiftIO $ feedInflate inf x
        fix $ \pop -> do
          mbs <- lift $ unsafeLiftIO popper
          case mbs of
            PRDone -> do
              rem' <- getUnused
              if B.null rem'
                then feeder
                else do
                  let len = B.length rem'
                  modIOVar index (\i -> i - len)
                  modIOVar pdata (B.dropEnd len)
                  flush
                  leftover' rem'
            PRNext bs -> do
              yield' $ Chunk bs
              pop
            PRError e -> lift $ throwM e
      Just Flush -> do
        flush
        yield' Flush
        feeder

unsafeLiftIO :: PrimMonad m => IO a -> m a
unsafeLiftIO = unsafePrimToPrim

---------------------------------------------
-- Utilities

either :: Either a b -> (a -> c) -> (b -> c) -> c
either e f g = P.either f g e

msb :: Word8 -> Bool
msb w8 = w8 .&. 128 /= 0

w8sTow32 ::  [Word8] -> Word32
w8sTow32 = foldl' f 0
  where
    f acc w8 = acc `shiftL` 8 .|. fromIntegral w8

getByte :: MonadResource m =>  ConduitT ByteString o m Word8
getByte = do
  byte <- headE
  case byte of
    Nothing -> liftIO $ throwM UnexpectedEndOfInput
    Just w8 -> return w8

getBytes :: MonadResource m => Int -> ConduitT ByteString o m [Word8]
getBytes n = replicateM n getByte

modIOVar :: (Env -> IORef a) -> (a -> a) -> ConduitT i o (M Env) ()
modIOVar getter fun = do
   field  <- asks getter
   liftIO $ modifyIORef' field fun

getIOVar :: (Env -> IORef a) -> ConduitT i o (M Env) a
getIOVar getter = do
  field <- asks getter
  liftIO $ readIORef field
