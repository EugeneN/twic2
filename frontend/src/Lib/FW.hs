{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecursiveDo           #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE CPP                   #-}

module Lib.FW (hostApp, appContainer) where

import Prelude
import Control.Applicative           ((<*>), (<$>))
import Control.Concurrent            (forkIO)
import Control.Monad                 (void)

import Data.Maybe                    (Maybe(..))
import Data.Monoid                   ((<>))

import qualified Reflex              as R
import qualified Reflex.Host.App     as RHA

import qualified Data.VirtualDOM     as VD
import qualified Data.VirtualDOM.DOM as DOM

import Types
import Lib.FRP


hostApp :: AppHost
hostApp appContainer anApp = do
  dombody     <- VD.getBody :: IO DOM.Node
  containerEl <- (VD.createElement VD.domAPI) "div"
  (VD.appendChild VD.domAPI) containerEl dombody

  R.runSpiderHost $ RHA.hostApp (appContainer containerEl anApp)

appContainer :: AppContainer t m l c
appContainer container anApp = do
  (vdomEvents, vdomSink) <- RHA.newExternalEvent

  (dynView, _) <- anApp
  curView <- R.sample $ R.current dynView

  let initialVDom = (Just curView, Nothing)

  vdomDyn <- R.foldDyn (\new (old, _) -> (Just new, old)) initialVDom (R.updated dynView)

  (R.updated vdomDyn) ~> vdomSink
  vdomEvents ~> draw

  whenReady . const $ vdomSink initialVDom

  where
    draw :: (l ~ DOM.Node) => (Maybe (VD.VNode l), Maybe (VD.VNode l)) -> IO ()
    draw (newVdom, oldVdom) = void . forkIO $ do
      print $ "draw " <> show newVdom
      VD.patch VD.domAPI container oldVdom newVdom
