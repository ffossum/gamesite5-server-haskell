{-# LANGUAGE OverloadedStrings #-}

module Websocket where

import qualified Network.WebSockets as WS
import Network.Wai.Handler.WebSockets
import Network.Wai
import Core.User (UserId)
import Network.HTTP.Types.Status
import qualified AuthCookie (userId)
import Control.Monad (forever)
import Data.Text (Text)
import Control.Concurrent.STM

fallbackApp :: Application
fallbackApp req respond = respond (responseLBS upgradeRequired426 mempty mempty)

wsServerApp :: WS.ServerApp
wsServerApp pendingConn = do
  let maybeUserId = AuthCookie.userId $ WS.requestHeaders (WS.pendingRequest pendingConn)
  conn <- WS.acceptRequest pendingConn
  WS.sendTextData conn $
    case maybeUserId of
      Just userId -> "hello user" :: Text
      Nothing -> "hello guest"
  WS.forkPingThread conn 30
  forever $ do
      msg <- WS.receiveDataMessage conn
      WS.sendDataMessage conn msg

mkWebsocketApp :: IO Application
mkWebsocketApp = do
  clientsVar <- newTVarIO []
  pure $ websocketsOr WS.defaultConnectionOptions wsServerApp fallbackApp

