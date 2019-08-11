{-# LANGUAGE OverloadedStrings #-}

module WebSocket.App
  ( mkWebsocketApp
  )
where

import qualified AuthCookie (userId)
import Control.Concurrent.STM (newTVarIO)
import Control.Exception (bracket)
import Control.Monad (forever, join)
import Core.User (UserId (..))
import qualified Core.User as User
import Data.Aeson (encode)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Encoding (encodeUtf8)
import Database.Redis (RedisChannel)
import qualified Database.Redis as Redis
import Network.HTTP.Types.Status
import Network.Wai (Application, responseLBS)
import Network.Wai.Handler.WebSockets (websocketsOr)
import qualified Network.WebSockets as WS
import UserService (UserService)
import qualified UserService
import WebSocket.Event (loginEvent)

mkWebsocketApp :: UserService IO -> Redis.Connection -> Redis.PubSubController -> IO Application
mkWebsocketApp userSvc redisConn pubSubCtrl = do
  clientsVar <- newTVarIO []
  pure $
    websocketsOr
      WS.defaultConnectionOptions
      (wsServerApp userSvc redisConn pubSubCtrl)
      fallbackApp
  where
    fallbackApp :: Application
    fallbackApp req respond = respond (responseLBS upgradeRequired426 mempty mempty)

wsServerApp :: UserService IO -> Redis.Connection -> Redis.PubSubController -> WS.ServerApp
wsServerApp userSvc redisConn pubSubCtrl pendingConn = do
  let maybeUserId = AuthCookie.userId $ WS.requestHeaders (WS.pendingRequest pendingConn)
  maybeUser <- join <$> traverse (UserService.getUser userSvc) maybeUserId
  conn <- WS.acceptRequest pendingConn
  WS.sendTextData conn $ encode (loginEvent maybeUser)
  WS.forkPingThread conn 30
  case maybeUser of
    (Just user) -> addUserChannelAndThen (User.userId user) pubSubCtrl $ echoForever conn
    Nothing -> echoForever conn

addUserChannelAndThen :: UserId -> Redis.PubSubController -> IO a -> IO a
addUserChannelAndThen (UserId uid) pubSubCtrl ioa =
  bracket (Redis.addChannelsAndWait pubSubCtrl [(channelName, handler)] [])
    id
    (const ioa)
  where
    handler :: ByteString -> IO ()
    handler bytes = putStrLn ("user " <> (show uid) <> " received msg: " <> show bytes)
    channelName :: RedisChannel
    channelName = encodeUtf8 $ "user:" <> Text.pack (show uid)

echoForever :: WS.Connection -> IO ()
echoForever conn =
  forever $ do
    msg <- WS.receiveDataMessage conn
    WS.sendDataMessage conn msg
