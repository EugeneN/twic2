{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE RecordWildCards       #-}

module BL.Core (
    Url
  , Tweet(..)
  , Author(..)
  , Username
  , ApiError(..)
  , BL.Types.Entities(..)
  , EntityUrl(..)
  , EntityMedia(..)
  , TweetElement(..)
  , JsonApiError(..)
  , JsonResponse(..)
  , TweetId
  , retweetStatusToTweet
  , handleIncomingFeedMessages
  , statusToTweet
  , saveLastSeen
  , saveLastSeenAsync
  , updateFeed
  , updateFeedSync
  , readUserstream
  , readUserInfo
  , retweetUrl
  , unretweetUrl
  , adhocTweetUrl
  , followUser
  , unfollowUser
  , getRunTime
  , tweetUrl
  , fetchContext
  , replyUrl
  , readHistory
  , getStatus
  , writeApi
  , sendFetchAccountRequest
  , readApi
  , starUrl
  , unstarUrl
  , twInfo
  , authorize
  , getAccessToken
  ) where

import           Data.Text                      (pack, unpack)

import           System.IO
import qualified Web.Authenticate.OAuth         as OA

import qualified Data.ByteString                as B
import qualified Data.ByteString.Char8          as B8
import qualified Data.ByteString.Lazy           as BSL
import           Network.HTTP.Conduit

import           Control.Exception.Lifted       (SomeException (..),
                                                 displayException, try)

import           Control.Applicative
import           Control.Concurrent             (MVar, ThreadId, forkIO,
                                                 killThread, modifyMVar_,
                                                 newEmptyMVar, newMVar, putMVar,
                                                 readMVar, swapMVar, takeMVar,
                                                 threadDelay, tryTakeMVar)
import           Control.Monad
import           Control.Monad.Trans            (liftIO)
import           Network.HTTP.Types             (Status (..))
import qualified Network.HTTP.Types             as HTTP

import qualified BL.CloudDataLayer              as CDL
import qualified BL.DataLayer                   as DL
import           BL.Instances
import           BL.Parser                      (parseTweet)
import           BL.Types
import qualified Config                         as CFG
import           Data.Aeson
import qualified Data.ByteString.Char8          as BS
import           Data.Conduit
import qualified Data.Conduit.List              as CL
import           Data.HashMap.Strict            hiding (lookup)
import           Data.Int                       (Int64)
import           Data.Text.Encoding             (decodeUtf8)
import           Data.Time.Clock                (NominalDiffTime, UTCTime (..),
                                                 diffUTCTime, getCurrentTime,
                                                 secondsToDiffTime)
import           GHC.Generics
import           Prelude                        hiding (error, id)
import           System.Log.Handler.Simple
import           System.Log.Logger
import           Web.Twitter.Conduit            hiding (lookup)
import           Web.Twitter.Conduit.Api        (usersShow)
import           Web.Twitter.Conduit.Base       (call)
import           Web.Twitter.Conduit.Parameters (UserParam (ScreenNameParam))
import           Web.Twitter.Types
import qualified Web.Twitter.Types              as TT

import           Control.Concurrent
import           System.Directory
import           Data.Maybe
import           Data.Aeson
import           Data.Aeson.Encode.Pretty

logRealm = "Core"

info  = infoM    logRealm
warn  = warningM logRealm
debug = debugM   logRealm
error = errorM   logRealm

oauthToken :: Cfg -> OAuth
oauthToken cfg = twitterOAuth { oauthConsumerKey = BS.pack (cfgOauthConsumerKey cfg)
                              , oauthConsumerSecret = BS.pack (cfgOauthConsumerSecret cfg) }

getAccessToken ::  MVar (AppState DL.MyDb) -> MVar (BS.ByteString, Credential) -> BS.ByteString -> BS.ByteString -> Cfg -> IO ()
getAccessToken rs credentialStore oauthToken' oauthVerifier cfg = do
    let auth = oauthToken cfg
    (t, cred) <- takeMVar credentialStore

    accessTokens <- withManager $ \m -> OA.getAccessToken auth (OA.insert "oauth_verifier" oauthVerifier cred) m
    
    case (lookup "oauth_token" $ unCredential accessTokens, lookup "oauth_token_secret" $ unCredential accessTokens) of
        (ot@(Just _), ots@(Just _)) -> do
            let cfg' = cfg { cfgAccessToken = BS.unpack <$> ot, cfgAccessTokenSecret = BS.unpack <$> ots }
            modifyMVar_ rs (\(s@RunState{..}) -> return $ s { conf = cfg' })
            BSL.writeFile CFG.userConfig $ encodePretty cfg'
        (_, _) -> debug "we have some problem"

authorize :: MVar (AppState DL.MyDb) -> MVar (BS.ByteString, Credential) -> Cfg -> IO (Either String LoginInfo)
authorize rs credentialStore cfg = do
    let auth = oauthToken cfg
    (cred :: Credential) <- withManager $ \m -> OA.getTemporaryCredential auth m

    case (cfgAccessToken cfg, cfgAccessTokenSecret cfg) of
        (Just _, Just _) -> return $ Right $ NotNeedAuth
        (Nothing, Nothing) -> 
            case lookup "oauth_token" $ unCredential cred of
                Just oauthToken -> do
                    let url = OA.authorizeUrl auth cred
                    debug $ "oauthToken " ++ show oauthToken
                    debug $ "URL " ++ show url
                    debug $ "Cred " ++ show cred

                    z <- tryPutMVar credentialStore (oauthToken, cred)

                    debug $ "tryPutMVar " ++ show z

                    return $ Right $ NeedAuth $ pack url

                Nothing -> return $ Left "problem with oauth_token"

oauthCredential :: Cfg -> Credential
oauthCredential cfg = OA.newCredential (B8.pack (fromJust $ cfgAccessToken cfg)) (B8.pack (fromJust $ cfgAccessTokenSecret cfg))

twInfo :: Cfg -> TWInfo
twInfo cfg = setCredential (oauthToken cfg) (oauthCredential cfg) def

retweetStatusToTweet :: TT.RetweetedStatus -> Tweet
retweetStatusToTweet s = Tweet { text               = parseTweet $ TT.rsText s
                               , created_at         = pack $ show $ TT.rsCreatedAt s
                               , id                 = fromIntegral (TT.rsId s) :: Integer
                               , id_str             = show $ TT.rsId s
                               , user               = statusUserToAuthor $ TT.rsUser s
                               , entities           = statusEntitiesToEntities $ TT.rsEntities s
                               , extendedEntities   = BL.Types.Entities [] [] Nothing
                               , retweet            = Just $ statusToTweet $ TT.rsRetweetedStatus s
                               , status_favorited   = Nothing
                               , status_retweeted   = Nothing
                               , BL.Types.statusInReplyToStatusId   = Nothing
                               , BL.Types.statusInReplyToUserId     = Nothing
                               , BL.Types.statusInReplyToScreenName = Nothing
                               }


statusToTweet :: TT.Status -> Tweet
statusToTweet s = Tweet { text              = parseTweet $ TT.statusText s
                        , created_at        = pack $ show $ TT.statusCreatedAt s
                        , id                = fromIntegral (TT.statusId s) :: Integer
                        , id_str            = show $ TT.statusId s
                        , user              = statusUserToAuthor $ TT.statusUser s
                        , entities          = statusEntitiesToEntities $ TT.statusEntities s
                        , extendedEntities  = statusEntitiesToEntities $ TT.statusExtendedEntities s
                        , retweet           = statusToTweet <$> TT.statusRetweetedStatus s
                        , status_favorited  = TT.statusFavorited s
                        , status_retweeted  = TT.statusRetweeted s
                        , BL.Types.statusInReplyToStatusId   = TT.statusInReplyToStatusId s
                        , BL.Types.statusInReplyToUserId     = TT.statusInReplyToUserId s
                        , BL.Types.statusInReplyToScreenName = TT.statusInReplyToScreenName s
                        }

statusUserToAuthor :: TT.User -> Author
statusUserToAuthor s = Author (TT.userName s)
                              (TT.userId s)
                              (TT.userScreenName s)
                              (TT.userDefaultProfileImage s)
                              (avatarUrl $ TT.userProfileImageURL s)
  where
  avatarUrl x = case x of
      Nothing -> "http://a0.twimg.com/sticky/default_profile_images/default_profile_6_normal.png"
      Just y -> unpack y


statusEntitiesToEntities :: Maybe TT.Entities -> BL.Types.Entities
statusEntitiesToEntities Nothing  = BL.Types.Entities [] [] Nothing
statusEntitiesToEntities (Just s) = BL.Types.Entities (xUrl <$> TT.enURLs s)
                                                      (xHashtag <$> TT.enHashTags s)
                                                      (Just $ xMedia <$> TT.enMedia s)
  where
  xUrl (TT.Entity x _) = EntityUrl (unpack $ TT.ueExpanded x) (unpack $ TT.ueURL x) [] (unpack $ TT.ueDisplay x)
  xHashtag (TT.Entity x _) = EntityHashtag (TT.hashTagText x) []
  xMedia (TT.Entity x _) = EntityMedia (unpack $ TT.meType x)
                                       []
                                       (unpack $ TT.ueExpanded $ TT.meURL x)
                                       (unpack $ TT.meMediaURL x)
                                       (unpack $ TT.meMediaURL x)
                                       (unpack $ TT.meMediaURL x)
                                       (xSizes $ TT.meSizes x)
  xSizes x = EntityMediaSizes (xSize $ x ! "thumb" )
                              (xSize $ x ! "large" )
                              (xSize $ x ! "medium" )
                              (xSize $ x ! "small" )
  xSize x = EntityMediaSize (TT.msHeight x) (TT.msWidth x) (unpack $ TT.msResize x)

--------------------------------------------------------------------------------

accountSettingsUrl :: Url
accountSettingsUrl = "https://api.twitter.com/1.1/account/settings.json"

retweetUrl :: TweetId -> Url
retweetUrl x = "https://api.twitter.com/1.1/statuses/retweet/" ++ show x ++ ".json"

unretweetUrl :: TweetId -> Url
unretweetUrl x = "https://api.twitter.com/1.1/statuses/unretweet/" ++ show x ++ ".json"

starUrl :: TweetId -> Url
starUrl x = "https://api.twitter.com/1.1/favorites/create.json?id=" ++ show x

unstarUrl :: TweetId -> Url
unstarUrl x = "https://api.twitter.com/1.1/favorites/destroy.json?id=" ++ show x

adhocTweetUrl :: TweetId -> Feed
adhocTweetUrl x = AdhocTweet $ "https://api.twitter.com/1.1/statuses/show/" ++ show x ++ ".json?include_my_retweet=1&include_entities=1"

tweetUrl :: TweetBody -> Url
tweetUrl status = "https://api.twitter.com/1.1/statuses/update.json?status=" ++ B8.unpack status

replyUrl :: TweetBody -> B8.ByteString -> Url
replyUrl status reply_to_id = "https://api.twitter.com/1.1/statuses/update.json?status=" ++ B8.unpack status
                                    ++ "&in_reply_to_status_id=" ++ B8.unpack reply_to_id

userTimeline :: Username -> Int -> Feed
userTimeline name 0 = UserTimeline $
  "https://api.twitter.com/1.1/statuses/user_timeline.json?screen_name=" ++ name
userTimeline name count = UserTimeline $
  "https://api.twitter.com/1.1/statuses/user_timeline.json?screen_name="
  ++ name ++ "&count=" ++ show count

homeTimeline :: Int -> Feed
homeTimeline 0 = HomeTimeline "https://api.twitter.com/1.1/statuses/home_timeline.json"
homeTimeline count = HomeTimeline $
  "https://api.twitter.com/1.1/statuses/home_timeline.json?count=" ++ show count

homeTimelineSince :: TweetId -> Feed
homeTimelineSince 0 = HomeTimeline "https://api.twitter.com/1.1/statuses/home_timeline.json"
homeTimelineSince tid = HomeTimeline $
  "https://api.twitter.com/1.1/statuses/home_timeline.json?count=200&since_id=" ++ show tid

homeTimelineSinceCount :: TweetId -> Int -> Feed
homeTimelineSinceCount 0 0 = HomeTimeline "https://api.twitter.com/1.1/statuses/home_timeline.json"
homeTimelineSinceCount tid 0 = HomeTimeline $
  "https://api.twitter.com/1.1/statuses/home_timeline.json?since_id=" ++ show tid
homeTimelineSinceCount 0 count = HomeTimeline $
  "https://api.twitter.com/1.1/statuses/home_timeline.json?count=" ++ show count
homeTimelineSinceCount tid count = HomeTimeline $
  "https://api.twitter.com/1.1/statuses/home_timeline.json?since_id="
  ++ show tid ++ "&count=" ++ show count

homeTimelineMaxidCount :: TweetId -> Int -> Feed
homeTimelineMaxidCount 0 0 = HomeTimeline
    "https://api.twitter.com/1.1/statuses/home_timeline.json?count=20"
homeTimelineMaxidCount tid 0 = HomeTimeline $
    "https://api.twitter.com/1.1/statuses/home_timeline.json?count=20&max_id=" ++ show tid
homeTimelineMaxidCount 0 count = HomeTimeline $
    "https://api.twitter.com/1.1/statuses/home_timeline.json?count=" ++ show count
homeTimelineMaxidCount tid count = HomeTimeline $
    "https://api.twitter.com/1.1/statuses/home_timeline.json?count=" ++ show count ++ "&max_id=" ++ show tid

getUpdateFeedUrl :: DL.MyDb -> String -> IO Feed
getUpdateFeedUrl db url = do
  xs <- CDL.readCloudDb url
  info $ "Read from cloud db with result of length: " ++ show (length <$> xs)

  let z = case xs of
          Left x   -> 0
          Right [] -> 0
          Right y  -> CDL.lastSeenId (maximum y)

  debug $ "Selected max tweet id: " ++ show z
  return $ homeTimelineSince z

--------------------------------------------------------------------------------

justTweets :: FeedState -> [Tweet]
justTweets xs = (\(TweetMessage t) -> t) <$> Prelude.filter justTweetMessage xs

justTweetMessage :: FeedMessage -> Bool
justTweetMessage (TweetMessage _) = True
justTweetMessage _                = False

saveLastSeenAsync :: DL.MyDb -> FeedState -> Cfg -> IO ThreadId
saveLastSeenAsync db ts cfg = forkIO $ saveLastSeen db ts cfg >> return ()

saveLastSeen :: DL.MyDb -> FeedState -> Cfg -> IO (Either CDL.CloudDataLayerError CDL.WriteResponse)
saveLastSeen _ ts _ | justTweets ts == [] = return $ Right $ CDL.WriteSuccess "write skipped"
saveLastSeen db ts cfg = do
    now <- getCurrentTime
    res <- CDL.writeCloudDb (CDL.CloudDbStoreItem (getMaxId $ justTweets ts) (show now)) cfg -- now
    info $ "Wrote to cloud db with result: " ++ show res
    return res

    where
    getMaxId :: [Tweet] -> TweetId
    --getMaxId [] = ?
    getMaxId ts = maximum (BL.Types.id <$> ts)

putToClientQueue q ms = do
    maybeOldMs <- tryTakeMVar q
    case maybeOldMs of
        Just oldms -> putMVar q $ oldms ++ ms
        Nothing    -> putMVar q ms

handleIncomingFeedMessages :: DL.MyDb -> MVar FeedState -> [FeedMessage] -> IO ()
handleIncomingFeedMessages _ _ [] = do
    info "Got no updated feed messagess, skipping saving"
    return ()
handleIncomingFeedMessages _ fv ts = do
    info $ "Got " ++ show (length ts) ++ " updated feed messages/s"
    putToClientQueue fv ts

readUserstream :: ScreenName -> Int -> Cfg -> IO (Either (ApiError String) FeedState)
readUserstream sn count cfg = do
    info $ "reading userstream where ScreenName=" ++ show sn ++ " and count=" ++ show count
    (_, res) <- readApi (BL.Core.userTimeline (unpack sn) count) cfg
    return ((TweetMessage <$>) <$> res)

--readuserInfo :: ScreenName -> IO (Either (ApiError HttpException) UserInfo)
readUserInfo sn cfg = withManager $ \mgr -> do
    res <- liftIO $ callWithResponse (twInfo cfg) mgr $ usersShow (ScreenNameParam sn)
    case Web.Twitter.Conduit.responseStatus res of
        HTTP.Status {statusCode = 200, statusMessage = _ } ->
            return $ Right $ Web.Twitter.Conduit.responseBody res -- res :: User

        HTTP.Status {statusCode=code, statusMessage=msg} ->
            return $ Left $ ApiError $ "Twitter API returned bad status code: " ++ show code
                                                                         ++ " " ++ show msg

followUser sn   = followUnfollowUser sn (friendshipsCreate  (ScreenNameParam sn))
unfollowUser sn = followUnfollowUser sn (friendshipsDestroy (ScreenNameParam sn))

followUnfollowUser sn req cfg = withManager $ \mgr -> do
    res0 <- liftIO $ callWithResponse (twInfo cfg) mgr req
    liftIO . debug . show $ Web.Twitter.Conduit.responseStatus res0

    case Web.Twitter.Conduit.responseStatus res0 of
        HTTP.Status {statusCode = 200, statusMessage = _ } -> do
            -- can't get rid of this 'cause on unfollow wtitter doesn't send unfollowed user info
            -- TODO handle errors
            res <- liftIO $ call (twInfo cfg) mgr $ usersShow (ScreenNameParam sn)
            return $ Right res -- res :: MonadResource User

        HTTP.Status {statusCode=code, statusMessage=msg} ->
            return $ Left $ ApiError $ "Twitter API returned bad status code: " ++ show code
                                                                         ++ " " ++ show msg



readHistory :: TweetId -> Int -> Cfg -> IO (Either (ApiError String) FeedState)
readHistory maxid count cfg = do
    info $ "reading history where maxid=" ++ show maxid ++ " and count=" ++ show count
    (_, res) <- readApi (homeTimelineMaxidCount maxid count) cfg
    return $ (TweetMessage <$>) <$> res

sendFetchAccountRequest :: MVar UTCTime -> IO ()
sendFetchAccountRequest accv = do
    now <- getCurrentTime
    debug $ "***Putting an account fetch request at " ++ show now
    _ <- forkIO $ putMVar accv now -- avoid blocking caller
    return ()

updateFeed :: MVar UTCTime -> IO ()
updateFeed uv = do
    now <- getCurrentTime
    debug $ "***Putting an update request at " ++ show now
    _ <- forkIO $ putMVar uv now -- avoid blocking caller
    return ()

updateFeedSync :: DL.MyDb -> MVar FeedState -> Cfg -> IO ()
updateFeedSync db fv cfg = do
    info "Updating feed"
    feedUrl <- getUpdateFeedUrl db (cfgCloudDbUrl cfg)
    doreq feedUrl db fv (0 :: Int)

  where
  doreq f db fv iter = do
    (_, res) <- readApi f cfg
    case res of
      Right ts -> handleIncomingFeedMessages db fv (reverse $ TweetMessage <$> ts)

      -- Not in scope: data constructor ‘FailedConnectionException2’
      -- Left (TransportError (FailedConnectionException2 _ _ _ ex)) -> if iter < CFG.updateRetryCount
      --   then do
      --     error $ "Http error at update attempt " ++ show iter ++ "/"++ show CFG.updateRetryCount ++ ": " ++ show ex
      --           ++ ". Retrying in " ++ show CFG.updateRetryDelay ++ "ms"
      --     threadDelay CFG.updateRetryDelay
      --     doreq f db fv (iter + 1)
      --
      --   else
      --     error $ "error: update retry count exceeded: error was: " ++ show ex

      Left (TransportError ex) -> error $ "error: transport error: " ++ show ex
      Left (ApiError msg)      -> error $ "error: api error: " ++ show msg

fetchContext :: MVar FeedState -> Cfg -> IO ()
fetchContext fv cfg = do
    res <- fetchSettings accountSettingsUrl cfg
    case res of
        Left err -> putToClientQueue fv [ErrorMessage err]
        Right settings -> do
            putToClientQueue fv [SettingsMessage settings]
            res' <- fetchFriends (accScreenName settings) cfg
            case res' of
                Left err' -> putToClientQueue fv [ ErrorMessage err' ]
                Right fs  -> putToClientQueue fv [ FriendsListMessage fs ]

    where
    fetchSettings :: Url -> Cfg -> IO (Either JsonApiError TASettings)
    fetchSettings url cfg = do
        req <- parseUrl url

        res <- try $ withManager $ \m -> do
                 signedreq <- OA.signOAuth (oauthToken cfg) (oauthCredential cfg) req
                 httpLbs signedreq m

        case res of
            Left e  -> return $ Left $ JsonApiError "" (pack $ show (e :: HttpException))
            Right r -> case eitherDecode $ Network.HTTP.Conduit.responseBody r of
                Left msg -> return $ Left $ JsonApiError "" $ pack msg
                Right s  -> return $ Right s

    fetchFriends :: ScreenName -> Cfg -> IO (Either JsonApiError [User])
    fetchFriends sn cfg = do
        -- TODO handle exceptions
        res <- withManager $ \mgr ->
            sourceWithCursor (twInfo cfg) mgr (friendsList (ScreenNameParam $ unpack sn)) $$ CL.consume


        return $ Right res

--         case res of
--             Left err -> return $ Left $ JsonApiError "" (decodeUtf8 err)
--             Right fs -> return $ Right fs

--         res <- callWithResponse twInfo mgr $ friendsList (ScreenNameParam $ unpack sn)
--         case Web.Twitter.Conduit.responseStatus res of
--             HTTP.Status {statusCode = 200, statusMessage = _ } ->
--                 return $ Right $ contents $ Web.Twitter.Conduit.responseBody res -- res :: [User]
--
--             HTTP.Status {statusCode=code, statusMessage=msg} ->
--                 return $ Left $ JsonApiError "" (decodeUtf8 msg)

writeApi :: Url -> Cfg -> IO (Either (ApiError _) FeedMessage)
writeApi url cfg = do
    req <- parseUrl url
    let req' = req { method = "POST" }
    res <- (try $ withManager $ \m -> do
                   signedreq <- OA.signOAuth (oauthToken cfg) (oauthCredential cfg) req'
                   httpLbs signedreq m) :: IO (Either SomeException (Network.HTTP.Conduit.Response BSL.ByteString))

    case res of
        Left e -> return $ Left $ ApiError $ "Other status exception: " ++ show e

        Right r -> case eitherDecode $ Network.HTTP.Conduit.responseBody r of
            Left msg -> return $ Left $ ApiError msg
            Right t  -> return $ Right $ TweetMessage t

readApi :: FromJSON a => Feed -> Cfg -> IO (Feed, Either (ApiError String) a)
readApi feed cfg = do
  req <- parseUrl $ unfeedUrl feed
  res <- (try $ withManager $ \m -> do
             signedreq <- OA.signOAuth (oauthToken cfg) (oauthCredential cfg) req
             httpLbs signedreq m) :: IO (Either SomeException (Network.HTTP.Conduit.Response BSL.ByteString))

  case res of
    -- Left (StatusCodeException resp _ ) ->
    --   let (HTTP.Status code msg) = Network.HTTP.Conduit.responseStatus resp
    --   in return (feed, Left $ ApiError $ "Twitter API returned bad status code: " ++ show code ++ " " ++ show msg)
    Left x ->
      return (feed, Left $ TransportError $ displayException x)

    Right r -> case eitherDecode $ Network.HTTP.Conduit.responseBody r of
      Left msg -> return (feed, Left $ ApiError msg)
      Right ts -> return (feed, Right ts)

  where
    unfeedUrl :: Feed -> Url
    unfeedUrl (UserTimeline u) = u
    unfeedUrl (HomeTimeline u) = u
    unfeedUrl (AdhocTweet u)   = u

getRunTime :: UTCTime -> IO NominalDiffTime
getRunTime st = do
    endTime <- getCurrentTime
    return $ diffUTCTime endTime st

-- TODO type aliases and type for status
getStatus :: UTCTime -> DL.MyDb -> Cfg -> IO (TweetId, UTCTime, NominalDiffTime)
getStatus st db cfg = do
    xs <- CDL.readCloudDb (cfgCloudDbUrl cfg)
    (_, prevTime) <- DL.getPrevState db
    rt <- getRunTime st

    let z = case xs of
              Left _   -> -1 -- unknown or error
              Right [] -> -2 -- cloud db empty
              Right y  -> CDL.lastSeenId (maximum y)

    return (z, prevTime, rt)
