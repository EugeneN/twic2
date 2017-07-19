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
import Control.Monad                 (void, join, sequence)
import Control.Monad.IO.Class        (liftIO)
import Control.Monad.Fix             (MonadFix)

import Data.Map.Strict               (Map)
import qualified Data.Map.Strict     as Map
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
type TheApp t m l       = (RHA.MonadAppHost t m, MonadFix m) => m (R.Dynamic t (VD.VNode l))
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
      threadDelay 10000 -- XXX FIXME getPostBuild
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

data AppBLAction     = AddCounter | RemoveCounter deriving (Show)
data AppCounterModel = AppCounterModel Int Int -- deriving (Show)
type ViewDyn t l     = R.Dynamic t (VD.VNode l)

-- the actual app
theApp :: TheApp t m l
theApp = do
  (controllerE :: R.Event t AppBLAction, controllerU) <- RHA.newExternalEvent
  (counterModelE :: R.Event t (Int -> Int), counterModelU) <- RHA.newExternalEvent
  (newInstancesModelE :: R.Event t [m (ViewDyn t l)], newInstancesModelU) <- RHA.newExternalEvent -- XXX ads MonadFix to the context

  updateModel controllerE (\x -> return (updateCounter x) >>= counterModelU)
  counterModelD <- R.foldDyn foldCounter (AppCounterModel 0 0) counterModelE :: m (R.Dynamic t AppCounterModel)

  let mas = fmap makeCounters (R.updated counterModelD) :: R.Event t (Map Int (Maybe (m (ViewDyn t l))))
  as <- RHA.holdKeyAppHost (Map.empty) mas              :: m (R.Dynamic t (Map Int (ViewDyn t l)))

  let as'           = fmap Map.elems as                         :: R.Dynamic t [ViewDyn t l]
      as''          = fmap mconcat as'                          :: R.Dynamic t (ViewDyn t l)
      jas           = join as''                                 :: R.Dynamic t (VD.VNode l)
      ownViewDyn    = fmap (render controllerU) counterModelD   :: R.Dynamic t (VD.VNode l)
      resultViewDyn = ownViewDyn <> jas                         :: R.Dynamic t (VD.VNode l)

  return resultViewDyn

  where
    updateCounter AddCounter    = (+1)
    updateCounter RemoveCounter = (\x -> if x - 1 < 0 then 0 else x - 1)

    foldCounter :: (Int -> Int) -> AppCounterModel -> AppCounterModel
    foldCounter op (AppCounterModel x y) = AppCounterModel (op x) x

    makeCounters :: (RHA.MonadAppHost t m) => AppCounterModel
                                           -> (Map Int (Maybe (m (ViewDyn t l))))
    makeCounters (AppCounterModel new old) =
      if new >= old
        then Map.singleton new (Just $ counterApp new)
        else Map.singleton new Nothing


    render :: Sink AppBLAction -> AppCounterModel -> VD.VNode l
    render controllerU (AppCounterModel new old) =
      panel [ button "-" greenButton [VD.On "click" (void . const (controllerU RemoveCounter))]
            , textLabel $ "Counters: " <> show new <> " (was: " <> show old <> ")"
            , button "+" greenButton [VD.On "click" (void . const (controllerU AddCounter))]
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
