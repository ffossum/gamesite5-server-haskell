{-# LANGUAGE OverloadedStrings #-}

module WebSocket.App (mkWebsocketApp) where

import qualified Network.WebSockets as WS
import Network.Wai.Handler.WebSockets
import Network.Wai
import Core.User (UserId(..))
import Network.HTTP.Types.Status
import qualified AuthCookie (userId)
import Control.Monad (forever, join)
import Data.Text (Text)
import Control.Concurrent.STM
import UserService
import WebSocket.Event (loginSuccessEvent)
import Data.Aeson (encode)

fallbackApp :: Application
fallbackApp req respond = respond (responseLBS upgradeRequired426 mempty mempty)

wsServerApp :: UserService IO -> WS.ServerApp
wsServerApp userSvc pendingConn = do
  let maybeUserId = AuthCookie.userId $ WS.requestHeaders (WS.pendingRequest pendingConn)
  maybeUser <- join <$> traverse (UserService.getUser userSvc) maybeUserId
  conn <- WS.acceptRequest pendingConn

  case maybeUser of
    Just user -> WS.sendTextData conn $ encode (loginSuccessEvent user)
    Nothing -> pure ()

  WS.forkPingThread conn 30

  forever $ do
      msg <- WS.receiveDataMessage conn
      WS.sendDataMessage conn msg

mkWebsocketApp :: UserService IO ->  IO Application
mkWebsocketApp userSvc = do
  clientsVar <- newTVarIO []
  pure $ websocketsOr WS.defaultConnectionOptions (wsServerApp userSvc) fallbackApp

