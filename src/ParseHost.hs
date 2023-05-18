{-# LANGUAGE OverloadedStrings #-}

module ParseHost (
    Host
  , HostParseError(..)
  , hostParser
  ) where

import Prelude hiding (last, init, null)
import Data.Attoparsec.Text (Parser, string, parseOnly, takeText )
import Control.Arrow        (left)
import Data.Text            (Text, init, null , last )

type Host        = Text

data HostParseError = Unsupported
                    | UrlPathMissing
  deriving Show

parseHost :: Parser Host
parseHost = do
  _ <- string "https"
  string "://" *> takeText

hostParser :: Text -> Either HostParseError Host
hostParser txt =
  let host = left (const Unsupported) . parseOnly parseHost
      ifNull host =
         if null host
         then Left UrlPathMissing
         else Right $ trimIfEndsInSlash host
  in host txt >>= ifNull
    where
      trimIfEndsInSlash txt = if last txt == '/' then init txt else txt
