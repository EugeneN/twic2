{-# LANGUAGE CPP                 #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE RecordWildCards     #-}

module UI.HTTP.App where

import           BL.Core                             (adhocTweetUrl, authorize,
                                                      fetchContext, followUser,
                                                      getAccessToken, getStatus,
                                                      readApi, readHistory,
                                                      readUserInfo,
                                                      readUserstream, replyUrl,
                                                      retweetUrl, saveLastSeen,
                                                      saveLastSeenAsync,
                                                      sendFetchAccountRequest,
                                                      starUrl, tweetUrl,
                                                      unfollowUser,
                                                      unretweetUrl, unstarUrl,
                                                      updateFeed, writeApi)
import           BL.DataLayer                        (MyDb)
import           BL.Types                            (ApiError, Cfg, Feed,
                                                      FeedMessage (..),
                                                      FeedState, Message (..),
                                                      ScreenName, Tweet,
                                                      TweetBody, TweetId,
                                                      UpdateMessage)
import           Blaze.ByteString.Builder            (Builder, fromByteString)
import           Config                              (heartbeatDelay)
import           Control.Applicative                 ((<$>))
import           Control.Concurrent                  (MVar, ThreadId, forkIO,
                                                      killThread, modifyMVar,
                                                      modifyMVar_, myThreadId,
                                                      newEmptyMVar, newMVar,
                                                      putMVar, readMVar,
                                                      takeMVar, threadDelay,
                                                      tryTakeMVar)
import           Control.Exception                   (fromException, handle)
import           Control.Monad                       (forM_, forever)
import           Control.Monad.IO.Class
import           Data.Aeson                          (encode)
import           Data.ByteString
import qualified Data.ByteString.Char8               as B8
import qualified Data.ByteString.Lazy                as BSL
-- import           Data.Int                            (Int64)
import           Data.Text                           (Text)
import qualified Data.Text                           as T
import           Data.Text.Encoding                  (decodeUtf8)
import           Data.Tuple
import           Data.UUID                           (UUID)
import           Data.UUID.V4                        (nextRandom)
import           Network.HTTP.Types                  (HeaderName, status200,
                                                      status302)
import           Network.HTTP.Types.Header           (ResponseHeaders)
import           Network.Wai                         (Application, pathInfo,
                                                      queryString, responseFile,
                                                      responseLBS,
                                                      responseStream)
import qualified Network.Wai.Handler.WebSockets      as WaiWS
import           Network.Wai.Util                    (redirect')
import qualified Network.WebSockets                  as WS
import           Prelude                             hiding (error)
import           System.Log.Handler.Simple
import           System.Log.Logger
import           Text.Blaze.Html.Renderer.Utf8       (renderHtmlBuilder)
import           UI.HTTP.Html                        (homePage)
import           UI.HTTP.Json                        (adhocToJson,
                                                      justFeedMessagesToJson,
                                                      justUnreadCountToJson,
                                                      justUserInfoToJson,
                                                      justUserToJson, loginJson,
                                                      retweetToJson, starToJson,
                                                      tweetToJson)

import           Blaze.ByteString.Builder.ByteString (fromByteString)
import           Data.FileEmbed                      (embedFile,
                                                      embedStringFile)
import           Data.Maybe                          (fromJust)
import           Data.Time.Clock                     (UTCTime (..))
import           Network.URI                         (parseURI)

import           Web.Authenticate.OAuth              (Credential)

import           BL.DataLayer
import           BL.Types

logRealm = "HttpApp"

info     = infoM logRealm
warn     = warningM logRealm
debug    = debugM logRealm
error    = errorM logRealm

type Filename  = String
type Client    = (UUID, WS.Connection)
type WSState   = [Client]


mimeHtml :: (HeaderName, ByteString)
mimeHtml = ("Content-Type", "text/html")
mimeText = ("Content-Type", "text/plain")
mimeJs   = ("Content-Type", "text/javascript")
mimeJSON = ("Content-Type", "application/json")
mimeIco  = ("Content-Type", "image/x-icon")
mimeGif  = ("Content-Type", "image/gif")


resMainjs     = $(embedFile "../webexe/Main.js")
-- resFavicon    = $(embedFile "webexe/favicon.ico")
-- resLoader     = $(embedFile "webexe/snake-loader.gif")
-- resLoaderDark = $(embedFile "webexe/snake-loader-darkbg.gif")

--embeddedHandler :: Application
embeddedHandler mime resourse request response =
  response $ responseLBS status200 mime bstr

  where
  bstr = BSL.fromStrict resourse


httpapp :: UTCTime -> MyDb -> Cfg -> MVar (B8.ByteString, Credential) -> MVar (AppState MyDb) -> Application -- = Request -> ResourceT IO Response
httpapp st db cfg credentialStore rs request sendResponse = do
  debug $ show $ pathInfo request

  RunState {..} <- readMVar rs

  case pathInfo request of
    []                  -> homeHandler request sendResponse

-- #ifdef XStaticResources
    ["cs", "Main.js"]           -> embeddedHandler [mimeJs]  resMainjs     request sendResponse
    -- ["favicon.ico"]             -> embeddedHandler [mimeIco] resFavicon    request sendResponse
    -- ["snake-loader.gif"]        -> embeddedHandler [mimeGif] resLoader     request sendResponse
    -- ["snake-loader-darkbg.gif"] -> embeddedHandler [mimeGif] resLoaderDark request sendResponse
-- #else
--     ["cs", "Main.js"]           -> staticHandler [mimeJs] "src/UI/HTTP/cs/res/Main.js" request sendResponse
--     ["favicon.ico"]             -> staticHandler [mimeIco] "src/UI/HTTP/cs/res/favicon.ico" request sendResponse
--     ["snake-loader.gif"]        -> staticHandler [mimeGif] "src/UI/HTTP/cs/res/snake-loader.gif" request sendResponse
--     ["snake-loader-darkbg.gif"] -> staticHandler [mimeGif] "src/UI/HTTP/cs/res/snake-loader-darkbg.gif" request sendResponse
-- #endif

    path -> case Prelude.head path of
      "retweet"  -> retweetHandler      conf request sendResponse
      "unretweet"-> unretweetHandler    conf request sendResponse
      "adhoc"    -> getAdhocTweetHandler conf request sendResponse
      "star"     -> starHandler         conf request sendResponse
      "unstar"   -> unstarHandler       conf request sendResponse
      "tweet"    -> tweetHandler        conf request sendResponse
      "reply"    -> replyHandler        conf request sendResponse
      "stat"     -> statHandler st db   conf request sendResponse
      "history"  -> historyHandler      conf request sendResponse
      "userfeed" -> userfeedHandler     conf request sendResponse
      "userinfo" -> userinfoHandler     conf request sendResponse
      "follow"   -> followHandler True  conf request sendResponse
      "unfollow" -> followHandler False conf request sendResponse
      "login"    -> loginHandler        rs credentialStore conf request sendResponse
      "callback" -> callbackHandler     rs credentialStore conf request sendResponse
      _          -> notFoundHandler         request sendResponse

makeClient :: UUID -> WS.Connection -> Client
makeClient a c = (a, c)

addClient :: Client -> WSState -> WSState
addClient client clients = client : clients

removeClient :: Client -> WSState -> WSState
removeClient client = Prelude.filter ((/= fst client) . fst)

sendToClients :: MyDb -> MVar [Client] -> FeedState -> Cfg -> IO ()
sendToClients db cs ts cfg = do
    clients <- readMVar cs
    case clients of
        [] -> info "No clients available"
        _ -> do
            saveLastSeenAsync db ts cfg
            broadcast (encode ts) clients

broadcast :: BSL.ByteString -> WSState -> IO ()
broadcast msg clients = forM_ clients $ \(_, conn) -> WS.sendTextData conn msg

app :: UTCTime -> MyDb -> MVar FeedState -> MVar UpdateMessage -> MVar UpdateMessage -> Cfg -> MVar (AppState MyDb)
    -> IO Application
app st db fv uv accv cfg rs = do
    cs <- newMVar ([] :: WSState)
    _ <- startBroadcastWorker db fv cs cfg
    (credentialStore :: MVar (B8.ByteString, Credential)) <- newEmptyMVar
    return $ WaiWS.websocketsOr WS.defaultConnectionOptions
                                (wsapp db fv uv accv cs)
                                (httpapp st db cfg credentialStore rs)

wsapp :: MyDb -> MVar FeedState -> MVar UpdateMessage -> MVar UpdateMessage -> MVar WSState
      -> WS.ServerApp
wsapp db fv uv accv cs pending = do
  conn <- WS.acceptRequest pending
  clientId <- nextRandom
  let client = makeClient clientId conn

  debug $ "<<~~ Incoming ws connection " ++ show clientId

  WS.forkPingThread conn heartbeatDelay
  modifyMVar_ cs $ \cur -> return $ addClient client cur
  sendFetchAccountRequest accv
  updateFeed uv
  trackConnection client cs

startBroadcastWorker :: MyDb -> MVar FeedState -> MVar WSState -> Cfg -> IO ThreadId
startBroadcastWorker db fv cs cfg = forkIO $
    forever $ do
        ts <- takeMVar fv
        sendToClients db cs ts cfg

trackConnection :: Client -> MVar WSState -> IO ()
trackConnection client@(clientId, clientConn) cs = handle catchDisconnect $
  forever $ do
    x <- WS.receive clientConn
    debug $ "?>?> got smth from ws client " ++ show clientId ++ ": " ++ show x
    -- TODO handle ControlMessage (Close 1001 "")

    where
      catchDisconnect e = case fromException e of
        Just WS.ConnectionClosed -> do
          warn $ "ws ConnectionClosed for " ++ show clientId
          filterOutClient client cs
          return ()

        Just (WS.CloseRequest a b) -> do
          warn $ "ws CloseRequest from client " ++ show clientId ++ ": " ++ show a ++ "/" ++ show b
          filterOutClient client cs
          return ()

        Just (WS.ParseException s) -> do
          error $ "ws ParseException for " ++ show clientId ++ ": " ++ s
          filterOutClient client cs
          return ()

        x -> do
          error $ "ws closed for unknown reason for " ++ show clientId ++ ": " ++ show x
          filterOutClient client cs
          return ()

      filterOutClient client cs =
        modifyMVar_ cs $ \clients ->
            return $ removeClient client clients

homeHandler :: Application
homeHandler request response = response $ responseStream status200 [mimeHtml] homeStream
    where
        homeStream :: (Builder -> IO ()) -> IO () -> IO ()
        homeStream send flush = (send . renderHtmlBuilder) homePage >> flush

staticHandler :: ResponseHeaders -> FilePath -> Application
staticHandler mime fn request response = response $ responseFile status200 mime fn Nothing

notFoundHandler :: Application
notFoundHandler request response = response $ responseLBS status200 [mimeText] "Unknown path"

getAdhocTweetHandler :: Cfg -> Application
getAdhocTweetHandler cfg request response = case queryString request of
    [("id", Just id_)] -> case B8.readInteger id_ of
          Just (int, str) -> do
            debug $ "got adhoc tweet request for: " ++ show id_
            response $ responseStream status200 [mimeJSON] (adhocTweetStream (fromIntegral int :: Integer))

          Nothing -> do
            error ("bad retweet id" :: String)
            response $ responseLBS status200 [mimeJSON] "bad adhoc id"

    _ -> response $ responseLBS status200 [mimeJSON] "bad request"

    where
        adhocTweetStream :: TweetId -> (Builder -> IO ()) -> IO () -> IO ()
        adhocTweetStream id_ send flush = do
            x :: (Feed, Either (ApiError String) Tweet) <- readApi (adhocTweetUrl id_) cfg
            send . adhocToJson . fmap (fmap TweetMessage . (: [])) . snd $ x
            flush
            -- readApi (adhocTweetUrl id_) cfg >>= send . adhocToJson . fmap (fmap TweetMessage) . snd >> flush

retweetHandler :: Cfg -> Application
retweetHandler cfg request response = case queryString request of
    [("id", Just id_)] -> case B8.readInteger id_ of
          Just (int, str) -> do
            debug $ "got retweet " ++ show id_
            response $ responseStream status200 [mimeJSON] (retweetStream (fromIntegral int :: Integer))

          Nothing -> do
            error ("bad retweet id" :: String)
            response $ responseLBS status200 [mimeJSON] "bad retweet id"

    _ -> response $ responseLBS status200 [mimeJSON] "bad request"

    where
        retweetStream :: TweetId -> (Builder -> IO ()) -> IO () -> IO ()
        retweetStream id_ send flush =
            writeApi (retweetUrl id_) cfg >>= send . retweetToJson >> flush

unretweetHandler :: Cfg -> Application
unretweetHandler cfg request response = case queryString request of
    [("id", Just id_)] -> case B8.readInteger id_ of
          Just (int, str) -> do
            debug $ "got unretweet " ++ show id_
            response $ responseStream status200 [mimeJSON] (unretweetStream (fromIntegral int :: Integer))

          Nothing -> do
            error ("bad retweet id" :: String)
            response $ responseLBS status200 [mimeJSON] "bad retweet id"

    _ -> response $ responseLBS status200 [mimeJSON] "bad request"

    where
        unretweetStream :: TweetId -> (Builder -> IO ()) -> IO () -> IO ()
        unretweetStream id_ send flush =
            writeApi (unretweetUrl id_) cfg >>= send . retweetToJson >> flush


starHandler :: Cfg -> Application
starHandler cfg request response = case queryString request of
    [("id", Just id_)] -> case B8.readInteger id_ of
          Just (int, str) -> do
            debug $ "got star " ++ show id_
            response $ responseStream status200 [mimeJSON] (starStream (fromIntegral int :: Integer))

          Nothing -> do
            error ("bad star id" :: String)
            response $ responseLBS status200 [mimeJSON] "bad star id"

    _ -> response $ responseLBS status200 [mimeJSON] "bad request"

    where
        starStream :: TweetId -> (Builder -> IO ()) -> IO () -> IO ()
        starStream id_ send flush =
            writeApi (starUrl id_) cfg >>= send . starToJson >> flush

unstarHandler :: Cfg -> Application
unstarHandler cfg request response = case queryString request of
    [("id", Just id_)] -> case B8.readInteger id_ of
          Just (int, str) -> do
            debug $ "got unstar " ++ show id_
            response $ responseStream status200 [mimeJSON] (unstarStream (fromIntegral int :: Integer))

          Nothing -> do
            error ("bad star id" :: String)
            response $ responseLBS status200 [mimeJSON] "bad star id"

    _ -> response $ responseLBS status200 [mimeJSON] "bad request"

    where
        unstarStream :: TweetId -> (Builder -> IO ()) -> IO () -> IO ()
        unstarStream id_ send flush =
            writeApi (unstarUrl id_) cfg >>= send . starToJson >> flush

historyHandler :: Cfg -> Application
historyHandler cfg request response = case queryString request of
    [("maxid", Just id_), ("count", Just count)] -> case B8.readInteger id_ of
          Just (int_id, str) -> do
            debug $ "got history request with maxid " ++ show int_id ++ " and count " ++ show count
            response $ responseStream status200 [mimeJSON] (historyStream (fromIntegral int_id :: Integer))

          Nothing -> do
            error ("bad history maxid" :: String)
            response $ responseLBS status200 [mimeJSON] "bad history id"

    _ -> response $ responseLBS status200 [mimeJSON] "bad request"

    where
        historyStream :: TweetId -> (Builder -> IO ()) -> IO () -> IO ()
        historyStream id_ send flush =
            readHistory id_ 20 cfg >>= send . justFeedMessagesToJson >> flush

userfeedHandler :: Cfg -> Application
userfeedHandler cfg request response = case queryString request of
    [("sn", Just screenName)] ->
        response $ responseStream status200 [mimeJSON] (userfeedStream (decodeUtf8 screenName))

    _ -> response $ responseLBS status200 [mimeJSON] "bad request"

    where
        userfeedStream :: ScreenName -> (Builder -> IO ()) -> IO () -> IO ()
        userfeedStream sn send flush =
            readUserstream sn 200 cfg >>= send . justFeedMessagesToJson >> flush

userinfoHandler :: Cfg -> Application
userinfoHandler cfg request response = case queryString request of
    [("sn", Just screenName)] ->
        response $ responseStream status200 [mimeJSON] (userinfoStream (decodeUtf8 screenName) cfg)

    _ -> response $ responseLBS status200 [mimeJSON] "bad request"

    where
        userinfoStream :: ScreenName -> Cfg -> (Builder -> IO ()) -> IO () -> IO ()
        userinfoStream sn cfg send flush =
            readUserInfo (T.unpack sn) cfg >>= send . justUserToJson >> flush

followHandler :: Bool -> Cfg -> Application
followHandler follow cfg request response = case queryString request of
    [("sn", Just screenName)] ->
        response $ responseStream status200 [mimeJSON] (followStream (decodeUtf8 screenName))

    _ -> response $ responseLBS status200 [mimeJSON] "bad request"

    where
        followStream :: ScreenName -> (Builder -> IO ()) -> IO () -> IO ()
        followStream sn send flush = if follow
            then followUser (T.unpack sn) cfg >>= send . justUserToJson >> flush
            else unfollowUser (T.unpack sn) cfg >>= send . justUserToJson >> flush

tweetHandler :: Cfg -> Application
tweetHandler cfg request response = case queryString request of
    [("status", Just status)] -> do
        debug "got tweet "
        debug $ show status
        response $ responseStream status200 [mimeJSON] (tweetStream status)

    _ -> response $ responseLBS status200 [mimeJSON] "bad request"

    where
        tweetStream :: TweetBody -> (Builder -> IO ()) -> IO () -> IO ()
        tweetStream status send flush =
            writeApi (tweetUrl status) cfg >>= send . tweetToJson >> flush

replyHandler :: Cfg -> Application
replyHandler cfg request response = case queryString request of
    [("status", Just status), ("reply_to", Just reply_to_id)] -> do
        debug "got reply"
        debug $ show status ++ " -> " ++ show reply_to_id
        response $ responseStream status200 [mimeJSON] (replyStream status reply_to_id)

    _ -> response $ responseLBS status200 [mimeJSON] "bad request"

    where
        replyStream :: TweetBody -> B8.ByteString -> (Builder -> IO ()) -> IO () -> IO ()
        replyStream status reply_to_id send flush =
            writeApi (replyUrl status reply_to_id) cfg >>= send . tweetToJson >> flush

loginHandler :: MVar (AppState MyDb) -> MVar (B8.ByteString, Credential) -> Cfg -> Application
-- loginHandler cfg request response = response =<< redirect' status302 [] . fromJust . parseURI =<< authorize cfg
loginHandler rs cs cfg request response = response $ responseStream status200 [mimeJSON] loginStream where
    loginStream :: (Builder -> IO ()) -> IO () -> IO ()
    loginStream send flush = do
        authorize rs cs cfg >>= send . loginJson >> flush

callbackHandler :: MVar (AppState MyDb) -> MVar (B8.ByteString, Credential) -> Cfg -> Application
callbackHandler rs cs cfg request response = case queryString request of
    [("oauth_token", Just oauthToken), ("oauth_verifier", Just oauthVerifier)] -> do
        debug $ show oauthToken ++ " -> " ++ show oauthVerifier
        getAccessToken rs cs oauthToken oauthVerifier cfg
        response =<< (redirect' status302 [] . fromJust . parseURI $ "http://localhost:3000")
        -- response $ responseStream status200 [mimeJSON] (callbackStream oauthToken oauthVerifier)
    _ -> response $ responseLBS status200 [mimeJSON] "bad request"

statHandler :: UTCTime -> MyDb -> Cfg -> Application
statHandler st db cfg request response = response $ responseStream status200 [mimeText] (respStream st db)
    where
    respStream :: UTCTime -> MyDb -> (Builder -> IO ()) -> IO () -> IO ()
    respStream st db send flush = do
        send (fromByteString "hello\n") >> flush
        getStatus st db cfg >>=  send . fromByteString . B8.pack . show >> flush
