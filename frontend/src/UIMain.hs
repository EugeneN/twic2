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
type AppContainer t m l c = (RHA.MonadAppHost t m, l ~ DOM.Node, c ~ Counter) => l -> TheApp t m l c -> m ()
type AppHost              = (forall t m l c . (l ~ DOM.Node, c ~ Counter) => AppContainer t m l c) -> (forall t m l c . c ~ Counter => TheApp t m l c) -> IO ()
type TheApp t m l c       = (RHA.MonadAppHost t m, MonadFix m) => m (R.Dynamic t (VD.VNode l), R.Dynamic t c)
type Sink a               = a -> IO Bool

socketUrl = "ws://localhost:3000"
-- socketUrl = "ws://echo.websocket.org"

--- Entry point ----------------------------------------------------------------

main = hostApp appContainer theApp
-- main = hostApp appContainer (counterApp 1 R.never)

--- Kernel ---------------------------------------------------------------------

hostApp :: AppHost
hostApp appContainer anApp = do
  dombody     <- VD.getBody :: IO DOM.Node
  containerEl <- (VD.createElement VD.domAPI) "div"
  (VD.appendChild VD.domAPI) containerEl dombody

  R.runSpiderHost $ RHA.hostApp (appContainer containerEl anApp)


(~>) :: RHA.MonadAppHost t m => R.Event t a -> (a -> IO b) -> m ()
(~>) ev sink = RHA.performEvent_ $ (liftIO . void . sink) <$> ev

appContainer :: AppContainer t m l c
appContainer container anApp = do
  (vdomEvents, vdomSink) <- RHA.newExternalEvent

  (dynView, _) <- anApp
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

panel ch = VD.h "div"
                (VD.prop [ ("class", "panel")
                         , ("style", "padding: 10px; border: 1px solid grey; width: auto; display: inline-block; margin: 5px;")])
                ch

subscribeToEvent ev f = RHA.performEvent_ $ fmap (liftIO . void . f) ev

--------------------------------------------------------------------------------

instance Monoid (VD.VNode l) where
  mempty      = VD.text ""
  mconcat as  = VD.h "div" (VD.prop []) as
  mappend a b = VD.h "div" (VD.prop []) [a, b]

data AppBLAction     = AddCounter | RemoveCounter | ResetAll deriving (Show, Eq)
data ChildAction     = Reset deriving (Show, Eq)
data AppCounterModel = AppCounterModel Int Int -- deriving (Show)
type ViewDyn t l     = R.Dynamic t (VD.VNode l)

-- the actual app
theApp :: TheApp t m l Counter
theApp = do
  (controllerE :: R.Event t AppBLAction, controllerU) <- RHA.newExternalEvent
  (counterModelE :: R.Event t (Int -> Int), counterModelU) <- RHA.newExternalEvent
  (childControllerE :: R.Event t ChildAction, childControllerU) <- RHA.newExternalEvent

  subscribeToEvent (R.ffilter onlyAddRemove controllerE) (\x -> return (updateCounter x) >>= counterModelU)
  counterModelD <- R.foldDyn foldCounter (AppCounterModel 0 0) counterModelE -- :: m (R.Dynamic t AppCounterModel)

  subscribeToEvent (R.ffilter (== ResetAll) controllerE) (\x -> childControllerU Reset)

  let mas = fmap (makeCounters childControllerE) (R.updated counterModelD) -- :: R.Event t (Map Int (Maybe (m (ViewDyn t l, R.Dynamic x))))
  as <- RHA.holdKeyAppHost (Map.empty) mas                     -- :: m (R.Dynamic t (Map Int (ViewDyn t l, R.Dynamic x)))

  let as' = fmap Map.elems as                                  -- :: R.Dynamic t [(ViewDyn t l, R.Dynamic x)]

      xs = fmap (fmap fst) as'                                 --  R.Dynamic t [ViewDyn t l]
      ys = fmap (fmap snd) as'                                 --  R.Dynamic t [R.Dynamic x]

      ys' = fmap mconcat ys                                    -- R.Dynamic t [R.Dynamic (Counter Int)]
      jys = join ys'                                           -- R.Dynamic (Counter Int)

      as'' = fmap mconcat xs                                   -- :: R.Dynamic t (ViewDyn t l)
      jas  = join as''                                         -- :: R.Dynamic t (VD.VNode l)

      allCounters = (,) <$> counterModelD <*> jys

      ownViewDyn    = fmap (render controllerU) allCounters    -- :: R.Dynamic t (VD.VNode l)
      resultViewDyn = ownViewDyn <> jas                        -- :: R.Dynamic t (VD.VNode l)

  return (resultViewDyn, pure (Counter 0))

  where
    onlyAddRemove AddCounter    = True
    onlyAddRemove RemoveCounter = True
    onlyAddRemove _             = False

    updateCounter AddCounter    = (+1)
    updateCounter RemoveCounter = (\x -> if x - 1 < 0 then 0 else x - 1)
    updateCounter _             = (+0)

    foldCounter :: (Int -> Int) -> AppCounterModel -> AppCounterModel
    foldCounter op (AppCounterModel x _) = AppCounterModel (op x) x

    makeCounters :: (RHA.MonadAppHost t m, Counter ~ c) =>
                    R.Event t ChildAction -> AppCounterModel -> (Map Int (Maybe (m (ViewDyn t l, R.Dynamic t c))))
    makeCounters childControllerE (AppCounterModel new old) =
      if new >= old
        then Map.singleton new (Just $ counterApp new childControllerE)
        else Map.singleton old Nothing

    render :: Sink AppBLAction -> (AppCounterModel, Counter) -> VD.VNode l
    render controllerU (AppCounterModel new old, Counter total) =
      panel [ panel [ button "-" greenButton [VD.On "click" (void . const (controllerU RemoveCounter))]
                    , textLabel $ "Counters: " <> show new <> " (was: " <> show old <> ")"
                    , button "+" greenButton [VD.On "click" (void . const (controllerU AddCounter))]
                    ]
            , panel [ textLabel $ "Total counters sum: " <> show total
                    , button "Reset all" redButton [VD.On "click" (void . const (controllerU ResetAll))]
                    ]
            ]

--------------------------------------------------------------------------------

-- top level business logic actions
data CounterBLAction = Inc | Dec deriving (Show)
data Counter         = Counter Int deriving (Show)

instance Monoid Counter where
  mempty = Counter 0
  mappend (Counter a) (Counter b) = Counter (a + b)

counterApp :: Int -> R.Event t ChildAction -> TheApp t m l Counter
counterApp id_ cmdE = do
  (blEvents, blSink) <- RHA.newExternalEvent
  (modelEvents :: R.Event t (Int -> Int), modelSink) <- RHA.newExternalEvent

  subscribeToEvent cmdE $ \ev -> case ev of
    Reset -> modelSink (*0) >> pure ()
    _     -> pure ()

  subscribeToEvent blEvents $ \ev -> case ev of
    Inc -> modelSink (+1)
    Dec -> modelSink (\x -> x - 1)

  modelDyn <- R.foldDyn (\op (Counter prev) -> Counter (op prev)) (Counter 0) modelEvents
  let dynView = fmap (render blSink) modelDyn

  return (dynView, modelDyn)

  where
    render :: Sink CounterBLAction -> Counter -> VD.VNode l
    render blSink (Counter c) =
      panel [ button "-" redButton [VD.On "click" (void . const (blSink Dec))]
            , textLabel $ "Counter #" <> show id_ <> ": " <> show c
            , button "+" redButton [VD.On "click" (void . const (blSink Inc))]
            ]
