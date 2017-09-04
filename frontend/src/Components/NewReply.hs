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

newReplyComponent :: (RHA.MonadAppHost t m, MonadFix m) => Sink Notification -> m (R.Dynamic t (VD.VNode l), Sink TweetAction)
newReplyComponent ntU = do
    (showE :: R.Event t Bool, showU) <- RHA.newExternalEvent -- show/hide form
    showD <- R.holdDyn False showE
    (queryE :: R.Event t TweetAction, queryU) <- RHA.newExternalEvent -- get info about user from tweet
    (modelE :: R.Event t ReplyInfo, modelU) <- RHA.newExternalEvent -- model
    modelD <- R.holdDyn (ReplyInfo "" "") modelE
    (submitE :: R.Event t (Bool, ReplyInfo), submitU) <- RHA.newExternalEvent -- submit
    -- submitD <- R.holdDyn (False, ("", "")) submitE

    subscribeToEvent submitE $ \s -> do
        ntU $ Info "newReplyComponent => subscribeToEvent" $ show s
        pure ()
        -- case s of
        --     -- (True, (status@(DL.null -> False), replyTo@(DL.null -> False))) -> do
        --     --     void . forkIO $ do
        --     --         print $ show (status, replyTo)
        --     --         -- liftIO . getAPI . JSS.pack $ "http://localhost:3000/reply?status=" <> status <> "&reply_to" <> replyTo
        --     --         pure ()
        --     (True, ri) -> print ("newReplyComponent => " <> show ri) >> pure ()
        --     _ -> pure ()

    subscribeToEvent queryE $ \r -> do
        showU True
        case r of
            Reply (t@BL.Tweet {..}) -> modelU (ReplyInfo "" (T.unpack $ BL.screen_name $ user)) >> pure ()

    -- subscribeToEvent queryE $ \r -> do
    --     case r of
    --         (Reply (BL.Tweet t)) -> do
    --             showU True
    --             void . forkIO $ do
    --                 liftIO . getAPI . JSS.pack $ "http://localhost:3000/reply?status=test&reply_to" <> (BL.screen_name $ BL.user t)
    --                 pure ()
    --         _ -> pure ()

    let v = (render showU modelU submitU) <$> showD <*> modelD

    return (v, queryU)

    where
        cont x = VD.h "div" (p_ [("id", "new-reply-container"), ("style", "margin-bottom:500px")]) [x]
        render _ _ _ False (ReplyInfo "" "") = cont mempty
        render showU modelU submitU True ri@(ReplyInfo {..}) = cont $
            block [ block [flip VD.with [VD.On "input" (\ev -> modelU (ri { status = JSS.unpack . jsval $ ev }) >> pure ())] $ VD.h "textarea" (VD.prop []) []]
                  , block [button "Reply" (p_ [("class", "new-reply")]) [ onClick_ $ submitU (True, ri) ]]]

