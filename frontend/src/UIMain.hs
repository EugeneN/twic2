{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecursiveDo           #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Main where

import Prelude
import Control.Applicative           ((<*>), (<$>))
import Control.Concurrent            (forkIO, threadDelay)
import Control.Monad                 (void, join)
import Control.Monad.IO.Class        (liftIO)

import Data.Maybe                    (Maybe(..))
import Data.Monoid
import qualified Data.Text           as T

import qualified Reflex              as R
import qualified Reflex.Class        as RC
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
-- main = hostApp appContainer (counterApp 1)

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
      print $ "draw " <> show newVdom
      VD.patch VD.domAPI container oldVdom newVdom


--- Userspace ------------------------------------------------------------------

redButton   = [("style", "background-color: red;   color: white; padding: 10px;")]
greenButton = [("style", "background-color: green; color: white; padding: 10px;")]

textLabel t = VD.h "span" (VD.prop [("style", "padding: 10px;")]) [VD.text t]

button label attrs listeners =
  flip VD.with listeners $
    VD.h "button" (VD.prop attrs) [VD.text label]

panel ch = VD.h "div" (VD.prop [("style", "padding: 10px;")]) ch

updateModel ev f = RHA.performEvent_ $ fmap (liftIO . void . f) ev

--------------------------------------------------------------------------------

instance Monoid (VD.VNode l) where
  mempty      = VD.text ""
  mconcat as  = VD.h "div" (VD.prop []) as
  mappend a b = VD.h "div" (VD.prop []) [a, b]

data AppBLAction = AddCounter | RemoveCounter deriving (Show)
data AppBLModel = AppBLModel  Int -- deriving (Show)
data ViewModel t l = ViewModel [R.Dynamic t (VD.VNode l)]

type ViewDyn t l = R.Dynamic t (VD.VNode l)

-- performEvent :: MonadAppHost t m => Event t (HostFrame t a) -> m (Event t a)

-- the actual app
theApp :: TheApp t m l
theApp = do
  (blEvents :: R.Event t AppBLAction,     blSink)    <- RHA.newExternalEvent
  (modelEvents :: R.Event t (Int -> Int), modelSink) <- RHA.newExternalEvent

  updateModel blEvents $ \ev -> case ev of
    AddCounter    -> modelSink (+1)
    RemoveCounter -> modelSink (\x -> if x - 1 < 0 then 0 else x - 1)

  modelDyn <- R.foldDyn (\op (AppBLModel prev) -> AppBLModel (op prev)) (AppBLModel 0) modelEvents -- RC.Dynamic t AppBLModel

  modelDyn' :: R.Dynamic t [m (ViewDyn t l)] <- R.foldDyn makeCounters [] (R.updated modelDyn)
  let modelDyn'' = fmap sequence modelDyn' :: R.Dynamic t (m [ViewDyn t l])

  runAH <- RHA.getRunAppHost

  z <- RHA.performEvent (fmap runAH $ R.updated modelDyn'') -- RC.Event t (HostFrame t (RHA.AppInfo t), [ViewDyn t l])
  let z' = fmap snd z :: R.Event t [ViewDyn t l]

  zDyn <- R.holdDyn [] z'                             :: m (R.Dynamic t [ViewDyn t l])
  let zDyn'         = fmap mconcat zDyn               :: R.Dynamic t (ViewDyn t l)
      jDyn          = join zDyn'                      :: R.Dynamic t (VD.VNode l)
      ownViewDyn    = fmap (render blSink) modelDyn   :: R.Dynamic t (VD.VNode l)
      resultViewDyn = ownViewDyn <> jDyn              :: R.Dynamic t (VD.VNode l)

  return resultViewDyn

  where
    makeCounters :: (RHA.MonadAppHost t m) => AppBLModel -> [m (RC.Dynamic t (VD.VNode l))] -> [m (RC.Dynamic t (VD.VNode l))]
    makeCounters (AppBLModel n) xs =
      let c = length xs
      in if n > c
        then xs <> [counterApp n]
        else take n xs

    render :: Sink AppBLAction -> AppBLModel -> VD.VNode l
    render blSink (AppBLModel c) =
      panel [ button "-" greenButton [VD.On "click" (void . const (blSink RemoveCounter))]
            , textLabel $ "Counters: " <> show c
            , button "+" greenButton [VD.On "click" (void . const (blSink AddCounter))]
            ]

--------------------------------------------------------------------------------

-- top level business logic actions
data CounterBLAction = Inc | Dec deriving (Show)
data CounterBLModel = Counter Int deriving (Show)

counterApp :: Int -> TheApp t m l
counterApp id_ = do
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
    render :: Sink CounterBLAction -> CounterBLModel -> VD.VNode l
    render blSink (Counter c) =
      panel [ button "-" redButton [VD.On "click" (void . const (blSink Dec))]
            , textLabel $ "Counter #" <> show id_ <> ": " <> show c
            , button "+" redButton [VD.On "click" (void . const (blSink Inc))]
            ]
