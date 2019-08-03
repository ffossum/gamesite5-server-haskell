{-# LANGUAGE OverloadedStrings #-}

module WebSocket.App
  ( mkWebsocketApp
  )
where

import qualified AuthCookie (userId)
import Control.Concurrent.STM
import Control.Monad (forever, join)
import Core.User (UserId (..))
import Data.Aeson (encode)
import Data.Text (Text)
import Network.HTTP.Types.Status
import Network.Wai
import Network.Wai.Handler.WebSockets
import qualified Network.WebSockets as WS
import UserService
import WebSocket.Event (loginEvent)

fallbackApp :: Application
fallbackApp req respond = respond (responseLBS upgradeRequired426 mempty mempty)

wsServerApp :: UserService IO -> WS.ServerApp
wsServerApp userSvc pendingConn = do
  let maybeUserId = AuthCookie.userId $ WS.requestHeaders (WS.pendingRequest pendingConn)
  maybeUser <- join <$> traverse (UserService.getUser userSvc) maybeUserId
  conn <- WS.acceptRequest pendingConn
  WS.sendTextData conn $ encode (loginEvent maybeUser)
  WS.forkPingThread conn 30
  forever $ do
    msg <- WS.receiveDataMessage conn
    WS.sendDataMessage conn msg

mkWebsocketApp :: UserService IO -> IO Application
mkWebsocketApp userSvc = do
  clientsVar <- newTVarIO []
  pure $ websocketsOr WS.defaultConnectionOptions (wsServerApp userSvc) fallbackApp
