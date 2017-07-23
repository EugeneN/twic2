{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module App (theApp) where

import Prelude
import Control.Applicative           ((<*>), (<$>))
import Control.Monad                 (void, join)

import Data.Map.Strict               (Map)
import qualified Data.Map.Strict     as Map
import Data.Maybe                    (Maybe(..))
import Data.Monoid

import qualified Reflex              as R
import qualified Reflex.Host.App     as RHA

import qualified Data.VirtualDOM     as VD

import qualified BL.Types           as BL
import  BL.Instances

import UIConfig
import Types
import Lib.FRP
import Lib.FW
import Lib.UI
import Components.Counter (counterApp)
import Components.Feed


data AppBLAction     = AddCounter | RemoveCounter | ResetAll deriving (Show, Eq)
data AppCounterModel = AppCounterModel Int Int
type ViewDyn t l     = R.Dynamic t (VD.VNode l)


theApp :: TheApp t m l Counter
theApp = do
  (controllerE :: R.Event t AppBLAction, controllerU) <- RHA.newExternalEvent
  (counterModelE :: R.Event t (Int -> Int), counterModelU) <- RHA.newExternalEvent
  (childControllerE :: R.Event t ChildAction, childControllerU) <- RHA.newExternalEvent

  subscribeToEvent (R.ffilter onlyAddRemove controllerE) $ counterModelU . updateCounter
  counterModelD <- R.foldDyn foldCounter (AppCounterModel 0 0) counterModelE    -- :: m (R.Dynamic t AppCounterModel)

  subscribeToEvent (R.ffilter (== ResetAll) controllerE) (\x -> childControllerU Reset)

  (testWSViewD, _) <- testWS

  let mas = fmap (makeCounters childControllerE) (R.updated counterModelD)      -- :: R.Event t (Map Int (Maybe (m (ViewDyn t l, R.Dynamic x))))
  as <- RHA.holdKeyAppHost (Map.empty) mas                                      -- :: m (R.Dynamic t (Map Int (ViewDyn t l, R.Dynamic x)))

  let as' = fmap Map.elems as                                                   -- :: R.Dynamic t [(ViewDyn t l, R.Dynamic x)]

      xs = fmap (fmap fst) as'                                                  --  R.Dynamic t [ViewDyn t l]
      ys = fmap (fmap snd) as'                                                  --  R.Dynamic t [R.Dynamic x]

      ys' = fmap mconcat ys                                                     -- R.Dynamic t [R.Dynamic (Counter Int)]
      jys = join ys'                                                            -- R.Dynamic (Counter Int)

      as'' = fmap mconcat xs                                                    -- :: R.Dynamic t (ViewDyn t l)
      jas  = join as''                                                          -- :: R.Dynamic t (VD.VNode l)

      allCounters = (,) <$> counterModelD <*> jys

      ownViewDyn    = fmap (render controllerU) allCounters                     -- :: R.Dynamic t (VD.VNode l)
      resultViewDyn = layout <$> ownViewDyn <*> testWSViewD <*> jas             -- :: R.Dynamic t (VD.VNode l)

  return (resultViewDyn, pure (Counter 0))

  where
    layout own testws counters =
      columns [(testws, 100)]

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
      panel [ panel [ button "-" (unA greenButton) [VD.On "click" (void . const (controllerU RemoveCounter))]
                    , textLabel $ "Counters: " <> show new <> " (was: " <> show old <> ")"
                    , button "+" (unA greenButton) [VD.On "click" (void . const (controllerU AddCounter))]
                    ]
            , panel [ textLabel $ "Total counters sum: " <> show total
                    , button "Reset all" (unA redButton) [VD.On "click" (void . const (controllerU ResetAll))]
                    ]
            ]
