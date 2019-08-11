{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (race_)
import Control.Exception (SomeException, catch)
import Control.Monad (forever)
import CryptoService (mkFakeCryptoService)
import qualified Database.Redis as Redis
import Lib
import Network.Wai.Handler.Warp
import Server (app)
import UserService (mkInMemoryUserService)
import WebSocket.App (mkWebsocketApp)

port :: Int
port = 8080

main :: IO ()
main = do
  redisConnection <-
    Redis.checkedConnect
      Redis.defaultConnectInfo
        { Redis.connectHost = "172.17.0.2"
        } <*
      putStrLn "Connected to redis"
  pubSubController <- Redis.newPubSubController [] []
  let cryptoSvc = mkFakeCryptoService
  userSvc <- mkInMemoryUserService cryptoSvc
  wsApp <- mkWebsocketApp userSvc
  putStrLn $ "warp " <> warpVersion
  putStrLn $ "Starting server listening on port " <> (show port)
  race_
    (run port (app userSvc wsApp))
    ( forever $
      Redis.pubSubForever
        redisConnection
        pubSubController
        (putStrLn "Redis pubsub active") `catch`
      ( \(err :: SomeException) ->
        putStrLn ("Redis pubsub error: " <> show err) *>
          threadDelay 10000
      )
    )
