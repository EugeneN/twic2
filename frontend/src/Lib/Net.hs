module Lib.Net (getAPI) where

import Data.Aeson
import Control.Exception
import Control.Monad.Except
import Data.JSString
import JavaScript.Web.XMLHttpRequest
import Data.ByteString
import Data.Monoid

getAPI :: FromJSON a => JSString -> IO (Either String a)
getAPI requestURI = do
  eitherResult <- liftIO (try $ xhrByteString (request requestURI) :: IO (Either XHRError (Response ByteString)) )
  return $ case eitherResult of
    Left s     -> Left  ("getAPI 1: " <> show s)
    Right result -> case contents result of
      Nothing          -> Left "getAPI 2: No response"
      Just byteString  -> case decodeStrict byteString of
        Nothing -> Left $  "getAPI 3: Parse error " <> show byteString
        Just x  -> Right x
  where
    request requestURI = Request { reqMethod          = GET
                                 , reqURI             = requestURI
                                 , reqLogin           = Nothing
                                 , reqHeaders         = []
                                 , reqWithCredentials = False
                                 , reqData            = NoData
                                 }
