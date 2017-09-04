{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ViewPatterns          #-}

module Components.NewReply where

import Control.Monad.Fix             (MonadFix)

import Control.Applicative           ((<*>), (<$>))
import Control.Concurrent            (forkIO)
import Control.Monad                 (void)
import Control.Monad.IO.Class        (liftIO)

import qualified Data.Text           as T
import qualified Reflex              as R
import qualified Reflex.Host.App     as RHA
import Types
import Data.Monoid

import qualified Data.VirtualDOM     as VD
import qualified Data.JSString       as JSS
import qualified BL.Types            as BL
import qualified Data.List           as DL

import Lib.FRP
import Lib.UI
import Lib.Net (getAPI)

-- 1 get user from original tweet -- query
-- 2 make status and prepare user name -- model
-- 3 show/hide form for reply -- show
-- status

data ReplyInfo = ReplyInfo { status :: String, replyTo :: String } deriving Show

type WidgetT r i o = Sink o -> i -> r
type Widget i o = forall l . WidgetT (VD.VNode l) i o

data Submit a = DontSubmit a | Submit a deriving Show

formComponent :: (RHA.MonadAppHost t m, MonadFix m, Show a) => a -> Widget a (Submit a) -> m (R.Dynamic t (VD.VNode l), R.Event t a)
formComponent z w = do
  (aE :: R.Event t (Submit a), aU) <- RHA.newExternalEvent
  aS <- R.holdDyn (DontSubmit z) aE
  let v = fmap (w aU . unpackSubmit) aS
  return (v, passSubmitsOnly aE)

  where
    unpackSubmit :: Submit a -> a
    unpackSubmit (DontSubmit x) = x
    unpackSubmit (Submit x)     = x

    passSubmitsOnly :: R.Reflex t => R.Event t (Submit a) -> R.Event t a
    passSubmitsOnly = R.fmapMaybe g
      where
        g (Submit x) = Just x
        g _          = Nothing

newReplyComponent :: (RHA.MonadAppHost t m, MonadFix m) => Sink Notification -> m (R.Dynamic t (VD.VNode l), Sink TweetAction)
newReplyComponent ntU = do
    (queryE :: R.Event t TweetAction, queryU) <- RHA.newExternalEvent -- get info about user from tweet

    (v, sE) <- formComponent "" w

    subscribeToEvent sE $ \x -> do
      print $ "Got submit: " <> x
      -- queryU $ ReplyInfo x ?

    return (v, queryU)

    where
        w :: Widget String (Submit String)
        w u x =
          cont $
             block [ block [flip VD.with [VD.On "input" (\ev -> void $ u (DontSubmit . JSS.unpack . jsval $ ev))]
                                         $ VD.h "textarea" (VD.prop []) [VD.text x] ]
                   , block [button "Reply" (p_ [("class", "new-reply")]) [ onClick_ $ u (Submit x) ]]
                   ]

        cont x = VD.h "div" (p_ [("id", "new-reply-container"), ("style", "margin-bottom:500px")]) [x]
