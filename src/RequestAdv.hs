{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE BlockArguments      #-}

module RequestAdv (protocol ) where

import Prelude hiding                  (take, maybe)
import Network.HTTP.Req
    ( Url,
      (/:),
      (=:),
      bsResponse,
      defaultHttpConfig,
      header,
      https,
      req,
      reqBr,
      responseBody,
      responseStatusCode,
      runReq,
      BsResponse,
      GET(GET),
      HttpException,
      NoReqBody(NoReqBody),
      POST(POST),
      ReqBodyBs(ReqBodyBs),
      Scheme(Https) )
import ParseHost                       (Host)
import Data.Text                       (Text, splitOn)
import Control.Exception
    ( handle, throw, throwIO, Exception, SomeException, IOException )
import Data.ByteString                 (ByteString)
import Data.Foldable                   (foldl')
import ParseAdv
    ( Advertisement(headId, capabs, refs), smartParser )
import BuildWant ( wants )
import Data.List                       (intersect)
import ParsePack                       (buildReader, Object(..), IData(..), IdxFile(..))
import qualified Prelude as P          (maybe)
import Data.ByteString.Lex.Integral ( readHexadecimal )
import Conduit
    ( MonadIO(liftIO),
      sinkFile,
      (.|),
      awaitForever,
      runConduitRes,
      yield,
      ConduitT,
      ZipSink(ZipSink, getZipSink),
      MonadThrow(throwM),
      MonadResource )
import Network.HTTP.Req.Conduit ( responseBodySource )
import Data.Conduit.Combinators ( dropE, fold, headE, takeE )
import ByteString.StrictBuilder
    ( builderBytes, bytes, word32BE, Builder )
import qualified Data.Vector                         as V
import qualified Data.Vector.Unboxed.Sized           as UV
import qualified Data.IntMap.Strict                  as I
import qualified Data.ByteString                     as B
import System.PosixCompat.Files
    ( createSymbolicLink,
      getFileStatus,
      getSymbolicLinkStatus,
      setFileMode,
      deviceID,
      fileGroup,
      fileID,
      fileMode,
      fileOwner,
      fileSize,
      modificationTime,
      statusChangeTime,
      FileStatus )
import qualified ParseObject                         as PO
import qualified Data.ByteString.Char8               as C
import Data.Finite ( finite )
import System.Directory
    ( createDirectory,
      createDirectoryIfMissing,
      removeDirectoryRecursive,
      renameFile )
import System.FilePath ( addExtension, (</>) )
import Crypto.Hash
    ( hashFinalize, hashInit, hashUpdate, hashWith, SHA1(..), Context )
import Data.ByteArray ( convert )
import Data.ByteString.Base16  (encode)
import Data.Int (Int64)
import Data.Word (Word64, Word32)
import ParseObject (Mode)
import Data.Coerce (coerce)
import Foreign.C.Types ( CTime(CTime) )
import System.PosixCompat.Types
    ( CDev(CDev),
      CGid(CGid),
      CIno(CIno),
      CMode(..),
      COff(COff),
      CUid(CUid) )

data RequestException = InvalidRepo

instance Show RequestException where
    show _ = "Repository not found"

instance Exception RequestException

data RefsParserException = BadRefFormat

instance Show RefsParserException where
  show _ = "Failed parseing refs advertisement"

instance Exception RefsParserException

data PackParseError = PackError

instance Show PackParseError where
  show _ = "Failed parsing pack response"

instance Exception PackParseError

data ServerError = ServerError

instance Show ServerError where
  show _ = "Failed parsing pack response"

instance Exception ServerError

data BuildingWorkingDirectory = ErrorBuildingWorkingDirectory

instance Show BuildingWorkingDirectory where
  show _ = "Error building Working Directory"

instance Exception BuildingWorkingDirectory


hostToUrl  :: Text -> Url 'Https
hostToUrl txt =
  let parts = splitOn "/" txt
      h     = head parts
      t     = tail parts
   in foldl' (/:) (https h) t

makeAdvRequest :: Host -> IO BsResponse
makeAdvRequest host = mkSmartRequest $ hostToUrl host

mkSmartRequest :: Url 'Https -> IO BsResponse
mkSmartRequest url =
  let host  = url /: "info" /: "refs"
      query = "service" =: ("git-upload-pack" :: Text)
  in handle (\(_ :: HttpException) -> throwIO InvalidRepo)
            $ runReq defaultHttpConfig
            $ req GET host NoReqBody bsResponse query

processResponse :: BsResponse -> IO ByteString
processResponse response = do
  let status = responseStatusCode response
  case status of
    200 -> pure $ responseBody response
    304 -> pure $ responseBody response
    _   -> throwIO InvalidRepo

getRefsAdvs :: Host -> IO ByteString
getRefsAdvs host = makeAdvRequest host >>= processResponse

readNak :: MonadResource m => ConduitT ByteString ByteString m ()
readNak = do
  nak <- takeE 8 .|  fold
  if nak == "0008NAK\n"
  then awaitForever yield
  else liftIO $ throwIO PackError

readPacketLines :: MonadResource m => ConduitT ByteString ByteString m ()
readPacketLines = do
  mlen <- takeE 4 .| fold
  case mlen of
    "0000" -> return ()
    mhex   -> do
      let len' = readHexadecimal @Int mhex
      maybe len' (liftIO $ throwIO PackError) $ \(len,_) -> do
        code <- headE
        case code of
         Just 1  -> takeE (len - 5)
         Just 2  -> dropE (len - 5)
         Just 3  -> liftIO $ throwIO ServerError
         _       -> liftIO $ throwIO PackError
        readPacketLines

protocol :: Host -> IO () -- (ByteString, ByteString)
protocol host = do
    references <- getRefsAdvs host
    case smartParser references of
      Left  _  -> throwIO BadRefFormat
      Right ad -> do
        let cps    = intersect ["multi_ack", "multi_ack_detailed","side-band", "side-band-64k", "ofs-delta"]
                               $ capabs ad
            (ws,_) = wants (refs ad) cps
        createBareRepo (repo </> ".git")
        handle (\(_ :: IOException ) -> putStrLn "hey") $ createDirectoryIfMissing True pathForPack
        (objs, idx) <- getPack ws (hostToUrl host)
        handle (\(_ :: SomeException) -> removeDirectoryRecursive repo >> putStrLn "I was here")
             do
          let indxBytes = serializeIdxFile idx
              idxHash   = convert $ hashWith SHA1 indxBytes
              filename  = hashFileName $ pyld idx
              hd        = headId ad
              bys       = findInIndex hd idx objs
          case bys of
            Nothing -> putStrLn "and here" >> throwM ErrorBuildingWorkingDirectory
            Just b  -> do
              case PO.commitParser b of
                Left _  -> putStrLn "or here" >> throwM ErrorBuildingWorkingDirectory
                Right c -> do
                  entries <- buildWorkingTree repo (PO.treeid c) idx objs
                  print entries 
                  B.writeFile (flip addExtension "idx" $ pathForPack </> filename) (indxBytes <> idxHash)
                  renameFile (pathForPack </> tmp_pack) (flip addExtension "pack" $ pathForPack </> filename)

repo :: String
repo = "/home/daniel/Desktop/test/"

createBareRepo :: FilePath -> IO ()
createBareRepo dir =
  mapM_ (\d -> createDirectoryIfMissing True (dir </> d)) directories
  where directories = ["objects", "refs", "hooks", "info"]

pathForPack :: String
pathForPack = repo </> ".git" </> "objects" </> "pack"

getPack :: ByteString -> Url 'Https -> IO (I.IntMap Object, IdxFile)
getPack needs url = do
  let opt = header "Content-Type" "application/x-git-upload-pack-request"
      route = url /: "git-upload-pack"
  runReq defaultHttpConfig $
    reqBr POST route (ReqBodyBs needs) opt $ \r ->
      runConduitRes $ responseBodySource r
                   .| readNak
                   .| readPacketLines
                   .| getZipSink (ZipSink (sinkFile (pathForPack </> tmp_pack)) *> ZipSink buildReader)

data IndexEntry = IndexEntry {
    ctime :: Int64
  , mtime :: Int64
  , dev   :: Word64
  , inode :: Word64
  , imode :: Word32
  , uid   :: Word32
  , gid   :: Word32
  , size  :: Int64
  , isha  :: ByteString
  , gmode :: Mode
  , path  :: String
  } deriving Show

indexEntry :: FilePath -> Mode -> ByteString -> FileStatus -> IndexEntry
indexEntry filepath gitMode sha' stat  =
  IndexEntry { ctime = coerce $ statusChangeTime stat
             , mtime = coerce $ modificationTime stat
             , dev   = coerce $ deviceID         stat
             , inode = coerce $ fileID           stat
             , imode = coerce $ fileMode         stat
             , uid   = coerce $ fileOwner        stat
             , gid   = coerce $ fileGroup        stat
             , size  = coerce $ fileSize         stat
             , isha  = sha'
             , gmode = gitMode
             , path  = filepath
             } 

buildWorkingTree :: String -> ByteString -> IdxFile -> I.IntMap Object -> IO [IndexEntry]
buildWorkingTree base key idxfile objects =
    let obj = maybe (findInIndex key idxfile objects) (Left mempty) PO.treeParser
      in case obj of
        Left _        -> throwM ErrorBuildingWorkingDirectory
        Right (PO.MkTree entries) -> do
          handle (\(e :: SomeException) -> print e >> throwM ErrorBuildingWorkingDirectory) 
              (concat <$> traverse (buildEntryTree base) entries)
  where
    buildEntryTree :: String -> PO.Entry -> IO [IndexEntry]
    buildEntryTree base' PO.Entry{..} =
      let subbase = base' </> C.unpack path
      in case mode of
        PO.Dir      -> handle (\(_ :: SomeException) -> throwM ErrorBuildingWorkingDirectory)
                      do
            createDirectory subbase
            ft <- getFileStatus subbase
            let entry = indexEntry subbase mode obid ft
            (entry :) <$> buildWorkingTree subbase obid idxfile objects
        PO.File     -> do
          case findInIndex obid idxfile objects of
            Nothing -> throw ErrorBuildingWorkingDirectory
            Just ob -> do
              B.writeFile subbase ob
              ft <- getFileStatus subbase
              let entry = indexEntry subbase mode obid ft 
              return [entry]
        PO.Exec     -> do
          case findInIndex obid idxfile objects of
            Nothing -> throwM ErrorBuildingWorkingDirectory
            Just ob -> do
              B.writeFile subbase ob
              setFileMode subbase $ toPosixMode [0,7,7,5]
              ft <- getFileStatus subbase
              let entry = indexEntry subbase mode obid ft 
              return [entry]
        PO.Link     ->
          case findInIndex obid idxfile objects of
            Nothing -> throwM ErrorBuildingWorkingDirectory
            Just ob -> do
              createSymbolicLink (C.unpack ob) subbase
              ft <- getSymbolicLinkStatus subbase 
              let entry = indexEntry subbase mode obid ft 
              return [entry]
        PO.GitLink -> do 
          createDirectory subbase
          ft <- getFileStatus subbase
          let entry = indexEntry subbase mode obid ft 
          return [entry]

toPosixMode :: [Int] -> CMode
toPosixMode = CMode . go (3 :: Int) 0
  where
    go _ acc []       = acc
    go n acc (x : xs) = go (n - 1) (acc + fromIntegral (8 ^ n * x)) xs

findInIndex :: ByteString -> IdxFile -> I.IntMap Object -> Maybe ByteString
findInIndex sha1 IdxFile{..} objects =
  let firstByte     = fromIntegral $ B.head sha1
      (idx1 , idx2) = (finite (firstByte - 1), finite firstByte)
      (begin, end)  = (fout `UV.index` idx1, fout `UV.index` idx2) -- search interval 
      slice         = V.slice (fromIntegral begin) (fromIntegral $ end - begin) pyld
      moff          = fromIntegral . off <$> V.find (\IData{..} -> sha == sha1) slice
   in gload <$> (moff >>= flip I.lookup objects)

serializeIdxFile :: IdxFile -> ByteString
serializeIdxFile IdxFile{..} =
       builderBytes
    $  word32BE sign
    <> word32BE vers
    <> UV.foldr' (\w32 b -> word32BE w32 <> b) mempty fout
    <> (\(x,y,z) -> x <> y <> z) (V.foldr f mempty pyld)
    <> bytes psha
  where
    f :: IData -> (Builder, Builder, Builder) -> (Builder, Builder, Builder)
    f IData{..} (b1, b2, b3) =
        (bytes sha <> b1, word32BE chc <> b2, word32BE off <> b3)

hashFileName :: V.Vector IData -> String
hashFileName v = ("pack-" <>) . C.unpack . encode . convert. hashFinalize  $  V.foldl' f hashInit v
  where
    f :: Context SHA1 -> IData -> Context SHA1
    f b a = hashUpdate b $ sha a


maybe :: Maybe a -> b -> (a -> b) -> b
maybe ma b f = P.maybe b f ma

tmp_pack :: String
tmp_pack = "tmp-pack.pack"


