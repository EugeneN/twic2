{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards     #-}
{-# LANGUAGE RecordWildCards   #-}

module BL.Worker where

import           Control.Concurrent

import           Web.Twitter.Conduit
import           Web.Twitter.Types.Lens

import           Control.Applicative
import           Control.Lens

import           Control.Lens.Action          (act, (^!))
import           Control.Monad.Trans.Resource (MonadResource)

import           Data.Aeson                   (FromJSON)
import qualified Data.Conduit.Combinators     as CC
import           Data.String                  (IsString)

import           Control.Monad
import           Control.Monad.IO.Class
import qualified Data.ByteString.Char8        as BS
import           Data.Conduit
import qualified Data.Conduit                 as C
import qualified Data.Conduit.Binary          as CB
import qualified Data.Conduit.Internal        as CI
import qualified Data.Conduit.List            as CL
import           Data.Monoid                  ((<>))
import qualified Data.Text                    as T
import qualified Data.Text.IO                 as T
import           Data.Time.Clock              (UTCTime (..), diffUTCTime,
                                               getCurrentTime,
                                               secondsToDiffTime)
import           Network.HTTP.Conduit         as HTTP
import           Web.Authenticate.OAuth

import qualified BL.Core                      as BLC
import           BL.DataLayer                 (MyDb, getPrevState, writeTime)
import           BL.Types
import qualified Config                       as CFG
import           Prelude                      hiding (error, lookup)
import           System.Log.Handler.Simple
import           System.Log.Logger
import qualified Web.Twitter.Conduit.Base     as WTCB
import qualified Web.Twitter.Conduit.Types    as WTCT
import qualified Web.Twitter.Types            as TT

logRealm = "Worker"

info = infoM logRealm
warn = warningM logRealm
debug = debugM logRealm
error = errorM logRealm

shouldForceUpdate :: UTCTime -> UTCTime -> Bool
shouldForceUpdate prev cur =
  diffUTCTime cur prev > realToFrac CFG.timeoutThreshod

-- TODO handle http timeouts, not just sleep mode
timeoutWorker :: MyDb -> MVar IPCMessage -> IO ThreadId
timeoutWorker db ch = forkIO $ forever $ do
  curTime <- getCurrentTime
  prevTime <- getPrevTimeFromDb db
  -- saveCurTimeToDb db curTime

  debug $ "... " ++ show prevTime ++ " : " ++ show curTime
                 ++ " / " ++ show (diffUTCTime curTime prevTime)
                 ++ " / " ++ show (realToFrac CFG.timeoutThreshod)

  if shouldForceUpdate prevTime curTime
    then sendUpdateMessage ch
    else debug "."

  threadDelay CFG.timeoutWorkerDelay

  where
  getPrevTimeFromDb db = do
    (_, oldPrevTime) <- getPrevState db
    return oldPrevTime

  sendUpdateMessage ch = putMVar ch MReloadFeed

saveCurTimeToDb = writeTime

accountFetchWorker :: MVar UpdateMessage -> MVar FeedState -> MVar (AppState MyDb) -> IO ThreadId
accountFetchWorker accv fv rs = forkIO $ forever $ do
    fetchreq <- takeMVar accv
    rs' <- readMVar rs
    debug $ "Got fetch account request at " ++ show fetchreq
    BLC.fetchContext fv (conf rs')

updateWorker :: MyDb -> MVar FeedState -> MVar UpdateMessage -> MVar (AppState MyDb) -> IO ThreadId
updateWorker db fv uv rs = forkIO $ forever $ do
    ur <- takeMVar uv
    rs' <- readMVar rs
    debug $ "*** Got an update request at " ++ show ur
    -- TODO throttle update requests here
    BLC.updateFeedSync db fv (conf rs')


-- copied from http://hackage.haskell.org/package/twitter-conduit-0.2.2.2/docs/src/Web-Twitter-Conduit-Stream.html#stream
-- and added code to record keep-alive messages from the api (\r\n),
-- which are expected to be sent every 30s
-- twitter api doc reccomend to wait 90s (3 keep-alive cycles),
-- and then reconnect immediately should no messages arrive
stream_ :: (MonadResource m)
        => TWInfo
        -> HTTP.Manager
        -> APIRequest apiName responseType
        -> m (ResumableSource m BS.ByteString)
stream_ info mgr req = do
    rsrc <- WTCB.getResponse info mgr =<< liftIO (WTCB.makeRequest req)
    return $ Web.Twitter.Conduit.responseBody rsrc

saveLatestMessageFromApi :: (Eq t, IsString t, Show t) => MyDb -> t -> IO ()
saveLatestMessageFromApi db x = do
  curTime <- getCurrentTime
  saveCurTimeToDb db curTime
  if x == "\r\n" -- aka twitter streaming api's in-band hearbeat protocol message
    then debug $ "❤❤❤❤ got heartbeet from streaming api @ " <> show curTime
    else debug $ "𝄞♪♫♬ got smth from streaming api @ " <> show curTime <> " " <> show x

streamWorker :: MyDb -> MVar FeedState -> MVar (AppState MyDb) -> IO ThreadId
streamWorker db m rs = do
  rs' <- readMVar rs
  forkIO $ withManager$ \mgr -> do
    rsrc <- stream_ (BLC.twInfo (conf rs')) mgr userstream

    let pass = passthroughSink (CL.mapM_ (liftIO . saveLatestMessageFromApi db)) (\_ -> pure ())
    let rsrc' = rsrc $=+ pass

    let rsrc'' = rsrc' $=+ CL.sequence WTCB.sinkFromJSON
    rsrc'' $$+- CL.mapM_ (^! act (liftIO . handleTL))

  where
  handleTL :: StreamingAPI -> IO ()
  handleTL (SStatus s)          = handleTweet $ BLC.statusToTweet s
  handleTL (SRetweetedStatus s) = handleTweet $ BLC.retweetStatusToTweet s
  handleTL (SEvent ev)          = handleEvent ev
  handleTL (SDelete s)          = debug $ "???? got SDelete: " ++ show s
  handleTL (SFriends s)         = debug $ "???? got SFriends: " ++ show s
  handleTL (SDirectMessage s)   = debug $ "???? got SDirectMessage: " ++ show s
  handleTL (SUnknown s)         = debug $ "???? got SUnknown: " ++ show s
  handleTL s                    = debug $ "???? got not a tweet: " ++ show s

  handleTweet t = BLC.handleIncomingFeedMessages db m [TweetMessage t]

  handleEvent TT.Event { evCreatedAt = _
                        , evTargetObject = _
                        , evEvent = "follow"
                        , evTarget = (ETUser user)
                        , evSource = _} = BLC.handleIncomingFeedMessages db m [UserMessage user]

  handleEvent x = debug $ "???? got unknown event: " ++ show x
