{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RecursiveDo           #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Main where

import Prelude
import Control.Applicative           ((<*>), (<$>))
import Control.Concurrent            (forkIO, threadDelay)
import Control.Monad                 (void, forever)
import Control.Monad.IO.Class        (liftIO)

import Data.Maybe                    (Maybe(..))
import Data.Monoid
import qualified Data.Text           as T

import qualified Reflex              as R
import qualified Reflex.Host.App     as RHA

import qualified Data.VirtualDOM     as VD
import qualified Data.VirtualDOM.DOM as DOM

import BL.Types                      (Tweet, Author, Entities, TweetElement)


-- `l` is DOM.Node in currently; polymorphic to enable other implementations
type AppContainer t m l = (RHA.MonadAppHost t m, l ~ DOM.Node) => l -> TheApp t m l -> m ()
type AppHost            = (forall t m l . l ~ DOM.Node => AppContainer t m l) -> (forall t m l . TheApp t m l) -> IO ()
type TheApp t m l       = (RHA.MonadAppHost t m) => m (R.Dynamic t (VD.VNode l))
type Sink a             = a -> IO Bool

socketUrl = "ws://localhost:3000"
-- socketUrl = "ws://echo.websocket.org"

--- Entry point ----------------------------------------------------------------

main = hostApp appContainer theApp

--- Kernel ---------------------------------------------------------------------

hostApp :: AppHost
hostApp appContainer anApp = do
  -- prepare DOM for the app
  dombody     <- VD.getBody :: IO DOM.Node
  containerEl <- (VD.createElement VD.domAPI) "div"
  (VD.appendChild VD.domAPI) containerEl dombody

  -- run the app
  R.runSpiderHost $ RHA.hostApp (appContainer containerEl anApp)


(~>) :: RHA.MonadAppHost t m => R.Event t a -> (a -> IO b) -> m ()
(~>) ev sink = RHA.performEvent_ $ (liftIO . void . sink) <$> ev

appContainer :: AppContainer t m l
appContainer container anApp = do
  (vdomEvents, vdomSink) <- RHA.newExternalEvent

  dynView <- anApp
  curView <- R.sample $ R.current dynView

  let initialVDom = (Just curView, Nothing)

  vdomDyn <- R.foldDyn (\new (old, _) -> (Just new, old)) initialVDom (R.updated dynView)

  (R.updated vdomDyn) ~> vdomSink
  vdomEvents ~> draw

  liftIO . void . forkIO  $ kickstart vdomSink initialVDom

  where
    kickstart sink val = do
      threadDelay 10000 -- XXX FIXME
      sink val
      pure ()

    draw :: (l ~ DOM.Node) => (Maybe (VD.VNode l), Maybe (VD.VNode l)) -> IO ()
    draw (newVdom, oldVdom) = void . forkIO $ do
      print "draw"
      VD.patch VD.domAPI container oldVdom newVdom


--- Userspace ------------------------------------------------------------------\

updateModel ev f = RHA.performEvent_ $ fmap (liftIO . void . f) ev

-- top level business logic actions
data BLAction = Inc | Dec deriving (Show)
data BLModel = Counter Int deriving (Show)

-- the actual app
theApp :: TheApp t m l
theApp = do
  -- business logic events
  (blEvents, blSink) <- RHA.newExternalEvent
  (modelEvents :: R.Event t (Int -> Int), modelSink) <- RHA.newExternalEvent

  updateModel blEvents $ \ev -> case ev of
    Inc -> modelSink (+1)
    Dec -> modelSink (\x -> x - 1)

  modelDyn <- R.foldDyn (\op (Counter prev) -> Counter (op prev)) (Counter 0) modelEvents

  -- dynamic value of rendered virtual ui
  let dynView = fmap (render blSink) modelDyn

  R.updated modelDyn ~> print

  -- return dynamic value of ui
  return dynView

  where
    redButton = [("style", "background-color:red; color: white; padding: 10px;")]

    textLabel t = VD.h "span" (VD.prop [("style", "padding: 10px;")]) [VD.text t]

    button label attrs listeners =
      flip VD.with listeners $
        VD.h "button" (VD.prop attrs) [VD.text label]

    panel ch = VD.h "div" (VD.prop [("style", "padding: 10px;")]) ch

    render :: Sink BLAction -> BLModel -> VD.VNode l
    render blSink (Counter c) =
      panel [ button "-" [("style", "background-color:red; color: white; padding: " <> show c <> "px;")] [VD.On "click" (void . const (blSink Dec))]
            , textLabel $ "<- resize buttons " <> show c <> " ->"
            , button "+" [("style", "background-color:red; color: white; padding: " <> show c <> "px;")] [VD.On "click" (void . const (blSink Inc))]
            ]
