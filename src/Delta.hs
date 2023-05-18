module Delta (patch) where

import Data.Serialize.Get
import Data.Bits hiding (shift)
import qualified Data.ByteString as B
import Data.Word (Word8)
import Control.Monad (foldM)
import Mason.Builder
import Data.Either (fromRight)

data Header = Header {
    sourceLength :: Int
  , targetLength :: Int
  , offset       :: Int
  }

patch :: B.ByteString -> B.ByteString -> Either String B.ByteString
patch base delta = do 
  header <- parseHeader delta 
  if B.length base == sourceLength header
    then runGet (run (offset header) base delta) delta 
    else Left "source length check failed"

run :: Int -> B.ByteString -> B.ByteString -> Get B.ByteString
run off source delta = do 
  skip off 
  cmd <- getWord8
  command cmd mempty source delta

parseHeader :: Monad m => B.ByteString -> m Header
parseHeader delta = do
    let res1 = runGet (decodeSize 0) delta
        (sourceSize, off) = fromRight (0,0)  res1
        res2 = runGet (decodeSize off) delta
        (targetSize, off') = fromRight (0,0)  res2
    return $ Header sourceSize targetSize off'
  where
    decodeSize off'' = do
      skip off''
      w8 <- getWord8
      next (mask w8) 7 w8 $ succ off''

    next base shift w8' count | msb w8' = do
      b <- getWord8
      let len = base .|. (mask b `shiftL` shift)
      next len (shift + 7) b $ succ count
    next truelen _ _ count = return (truelen, count)
    mask byte = fromIntegral $ byte .&. 127


command :: Word8 -> BuilderFor StrictByteStringBackend -> B.ByteString -> t -> Get B.ByteString
command instruction acc source delta = do
    result <- choose instruction
    isEnd  <- isEmpty
    let acc' = acc <> byteString result
    if isEnd
      then return $ toStrictByteString acc'
      else do
        cmd' <- getWord8
        command cmd' acc' source delta
  where
    choose opcode | msb opcode = copy opcode source
    choose opcode              = insert opcode

msb :: Word8 -> Bool
msb w8 = (w8 .&. 128) /= 0

insert :: Integral a => a -> Get B.ByteString
insert = getByteString . fromIntegral

copy :: Word8 -> B.ByteString -> Get B.ByteString
copy opcode source = do
    (off, len) <- readCopyInstruction opcode
    return $ copy' len off source
  where
    copy' len' off' = B.take len' . B.drop off'

readCopyInstruction :: (Integral a, Bits a) => Word8 -> Get (a, a)
readCopyInstruction opcode = do
    off <- foldM readIfBitSet 0 $ zip [0x01, 0x02, 0x04, 0x08] [0, 8 ..]
    len <- foldM readIfBitSet 0 $ zip [0x10, 0x20, 0x40]       [0, 8 ..]
    let l = if len == 0 then 0x10000 else len
    return (off,l)
  where
    calculateVal off' shift =
      if shift /= 0
        then (\x -> off' .|. (x `shiftL` shift)) . fromIntegral
        else fromIntegral

    readIfBitSet off' (test, shift) =
      if opcode .&. test /= 0
        then fmap (calculateVal off' shift) getWord8
        else return off'
