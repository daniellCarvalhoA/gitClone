{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Main (main) where

import RequestAdv
import ParseCommandLine 
-- import qualified Data.ByteString as B 
-- import Control.Arrow 
-- import ParseAdv ( refs, Advertisement (..))
-- import BuildWant
-- import RequestPack (getPack)
-- import Network.HTTP.Req


main :: IO ()
main = do
 host <- parseCommandLine 
 -- refAdv  <- getRefsAdvs h 
 protocol host

 -- case parseAdv refAdv of 
 --    Left _    -> putStrLn "Error parsing ref advertisement"
 --    Right Advertisement{..} -> 
 --      let (ws,_) = wants refs []
 --        in getPack ws $ foldUrl https host
