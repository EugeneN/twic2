{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Lib.WebSocket where

import Prelude
import Control.Applicative           ((<*>), (<$>))
import Control.Concurrent            (forkIO, threadDelay)
import Control.Concurrent.MVar       (newEmptyMVar, putMVar, tryReadMVar)
import Control.Monad                 (void)
import Control.Monad.IO.Class        (liftIO)

import qualified Data.Aeson          as A
import Data.Maybe                    (Maybe(..), isNothing)
import Data.Monoid                   ((<>))
import qualified Data.Text           as T
import qualified Data.Text.Encoding  as TE

import qualified Reflex              as R
import qualified Reflex.Host.App     as RHA


import qualified JavaScript.Web.WebSocket as WS
import qualified JavaScript.Web.MessageEvent as ME
import qualified Data.JSString      as JSS
import qualified Data.JSString.Text as JST

import Types
import Lib.FRP
import qualified BL.Types           as BL
import BL.Instances


setupWebsocket :: RHA.MonadAppHost t m => String -> m (WSInterface t, R.Dynamic t (Maybe WS.WebSocket))
setupWebsocket socketUrl = do
  (wsRcvE :: R.Event t (Either String WSData), wsRcvU) <- RHA.newExternalEvent
  (wsE :: R.Event t (Maybe WS.WebSocket), wsSink) <- RHA.newExternalEvent

  x <- liftIO $ newEmptyMVar

  let wscfg = WS.WebSocketRequest (JSS.pack socketUrl) []
                                  (Just $ \ev -> putMVar x Nothing >> wsSink Nothing >> print "ws closed"  )
                                  (Just $ \ev -> (wsRcvU . decodeWSMsg $ ev) >> pure () )

  liftIO . void $ connectWS wscfg x wsSink 0

  wsD' <- R.holdDyn Nothing wsE
  subscribeToEvent' (R.ffilter isNothing wsE) .
    const . liftIO . void $ connectWS wscfg x wsSink 1000000

  let wssend = \payload -> do
                  wsh <- tryReadMVar x
                  case wsh of
                    Just (Just wsh') -> WS.send (encodeWSMsg payload) wsh' >> pure (Right True)
                    _                -> return $ Left "ws not ready"

  let wsi = WSInterface { ws_rcve = wsRcvE
                        , ws_send = wssend }

  return (wsi, wsD')

connectWS wscfg x wsSink delay =
  forkIO $ void $ do
    print $ "(re)connecting websocket in " <> show delay <> "ns"
    threadDelay delay
    ws <- WS.connect wscfg
    putMVar x $ Just ws
    wsSink $ Just ws

encodeWSMsg :: WSData -> JSS.JSString
encodeWSMsg (WSData fm) = error "this action is not intended to be used"
encodeWSMsg (WSCommand s) = JSS.pack s

decodeWSMsg :: ME.MessageEvent -> Either String WSData
decodeWSMsg m =
  case ME.getData m of
    ME.StringData s       -> WSData <$> (A.eitherDecodeStrict . TE.encodeUtf8 . JST.textFromJSString $ s :: Either String BL.FeedState)
    ME.BlobData _         -> Left "BlobData not supported yet"
    ME.ArrayBufferData _  -> Left "ArrayBufferData not supported yet"
