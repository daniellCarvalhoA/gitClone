{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE OverloadedStrings #-}

module ParseAdv (
     Advertisement(..)
   , Ref (..)
   , smartParser
   , dumbParser
   )where

import Data.Attoparsec.ByteString
import qualified Data.Attoparsec.ByteString.Char8 as AC
-- import qualified Data.Attoparsec.ByteString       as B
import qualified Data.ByteString.Char8            as C
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Control.Monad (void)
import Data.ByteString.Base16 (decode)

data Advertisement = Advertisement
        { headId :: ByteString
        , headpt :: C.ByteString
        , capabs :: [C.ByteString]
        , refs   :: [Ref]
        } deriving Show

data Ref = Ref
        { refid :: C.ByteString
        , ref   :: C.ByteString
        } deriving Show


smartParser :: ByteString -> Either String Advertisement
smartParser = parseOnly advParser

dumbParser :: ByteString -> Either String Advertisement
dumbParser = parseOnly $ extracthead parseRefs
  where
    extracthead :: Parser [Ref] -> Parser Advertisement
    extracthead pref = do
          ls  <- pref
          case ls of
            [] -> fail "No references found"
            (Ref{..} : xs) -> return $ Advertisement {headId = refid
                                                    , headpt = ref
                                                    , capabs = []
                                                    , refs   = xs }
advParser :: Parser Advertisement
advParser = do
  parseService <* AC.char '\n'
  flushParser
  hd <- parseHead <* AC.space
  hp <- parseHeadPath
  cp <- parseCapabilities <* AC.char '\n'
  rf <- parseRefs <* AC.char '\n'
  flushParser
  return $ Advertisement {headId=hd, headpt=hp, capabs=cp, refs=rf}

parseHead :: Parser B.ByteString
parseHead = do
  b16 <- B.drop 4 <$> AC.takeTill (== ' ')
  case decode b16 of
    Left m -> fail m
    Right bytes -> return bytes

parseHeadPath :: Parser C.ByteString
parseHeadPath = AC.takeTill (== '\0')

parseCapabilities :: Parser [C.ByteString]
parseCapabilities = C.split ' ' <$> AC.takeTill (== '\n')

parseRef :: Parser Ref
parseRef =
      (Ref . C.drop 4 <$> (AC.takeTill (== ' ') <* AC.space ))
  <*> AC.takeTill (== '\n')

parseRefs :: Parser [Ref]
parseRefs = parseRef `AC.sepBy` AC.char '\n'

parseService :: Parser ()
parseService = do
  serviceLine <- C.split ' ' <$> AC.takeTill (== '\n')
  case serviceLine of
    [x,y] | validate5 x && y == "service=git-upload-pack" -> pure ()
    _ -> fail "Invalid service line"

validate5 :: C.ByteString -> Bool
validate5 five =
     C.length five == 5
  && C.all (AC.inClass allowed) (C.take 4 five)
  && C.last five == '#'
  where
    allowed = "0-9a-f"

flushParser :: Parser ()
flushParser = void $ string "0000"

