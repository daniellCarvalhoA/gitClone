{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module BuildWant where

import qualified Data.ByteString.Char8 as C
import Control.Arrow 
import ParseAdv
import Data.Function ((&))
import Data.Foldable (foldl')
import Numeric (showHex)
import Text.Printf (printf)

wants :: [Ref] -> [C.ByteString] -> (C.ByteString, C.ByteString )
wants rfs capabilities = rfs
       & filter predicate
       & fmap refid &&& toPackedForm 
       & ((<> flushPacket <> donePacket) . foldl' fun "") *** accum 

  where
    accum =
        C.intercalate "\n"
      . map (\Ref{..} -> refid <> " " <> ref)

    fun acc i | C.null acc  = 
      acc <> pktLine ("want" <> " " <> i <> " " <> C.unwords capabilities <> "\n" )
              | otherwise   = 
      acc <> pktLine ("want" <> " " <> i <> "\n")  

    predicate1 (Ref _ path) = 
             or  
         $  ($ path) 
        <$> [C.isPrefixOf "refs/heads", C.isPrefixOf "refs/tags"]

    predicate2  (Ref _ path) = 
        not $ C.isSuffixOf "^{}" path

    predicate rf = predicate1 rf && predicate2 rf

pktLine :: C.ByteString -> C.ByteString 
pktLine l = hex <> l  
  where 
      len = 4 + C.length l 
      pre = showHex len ""
      hex = C.pack $ printf "%04s" pre 

toPackedForm :: [Ref] -> [Ref]
toPackedForm = fmap remotes 
  where
    remotes (Ref rid path) = 
      Ref rid $  
        case C.split '/' path of 
          ["refs","heads",rest] -> "refs/remote/origin/" <> rest 
          _                     -> path 

flushPacket :: C.ByteString 
flushPacket = "0000" 

donePacket :: C.ByteString 
donePacket = pktLine "done\n" 




