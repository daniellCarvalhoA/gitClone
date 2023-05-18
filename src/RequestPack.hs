{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds         #-}
module RequestPack where 

import Prelude hiding (print)
import Conduit
import Network.HTTP.Req.Conduit
import Network.HTTP.Req
import Data.ByteString (ByteString)
import Data.Conduit.Combinators


getPack :: ByteString -> Url 'Https -> IO ()
getPack needs url = do 
  let opt = header "Content-Type" "application/x-git-upload-pack-request"
      route = url /: "git-upload-pack"
  runReq defaultHttpConfig $ 
    reqBr POST route (ReqBodyBs needs) opt $ \r -> 
      runConduitRes $ 
        responseBodySource r .| getZipSink (ZipSink print  <* ZipSink (sinkFile "tmp-pack.pack"))












