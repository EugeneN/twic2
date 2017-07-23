{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Components.Counter (counterComponent) where

import Prelude
import Control.Monad                 (void)
import Data.Monoid
import qualified Reflex              as R
import qualified Reflex.Host.App     as RHA
import qualified Data.VirtualDOM     as VD

import Types
import Lib.FRP
import Lib.UI


data CounterBLAction = Inc | Dec deriving (Show)
-- data Counter         = Counter Int deriving (Show)

instance Monoid Counter where
  mempty = Counter 0
  mappend (Counter a) (Counter b) = Counter (a + b)

counterComponent :: Int -> R.Event t ChildAction -> TheApp t m l Counter
counterComponent id_ cmdE = do
  (blEvents, blSink) <- RHA.newExternalEvent
  (modelEvents :: R.Event t (Int -> Int), modelSink) <- RHA.newExternalEvent

  subscribeToEvent cmdE $ \ev -> case ev of
    Reset -> modelSink (*0) >> pure ()

  subscribeToEvent blEvents $ \ev -> case ev of
    Inc -> modelSink (+1)
    Dec -> modelSink (\x -> x - 1)

  modelDyn <- R.foldDyn (\op (Counter prev) -> Counter (op prev)) (Counter 0) modelEvents
  let dynView = fmap (render blSink) modelDyn

  return (dynView, modelDyn)

  where
    render :: Sink CounterBLAction -> Counter -> VD.VNode l
    render blSink (Counter c) =
      panel [ button "-" (unA blueButton) [VD.On "click" (void . const (blSink Dec))]
            , textLabel $ "Counter #" <> show id_ <> ": " <> show c
            , button "+" (unA blueButton) [VD.On "click" (void . const (blSink Inc))]
            ]
