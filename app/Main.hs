module Main where

import Lib
import Server (app)
import Network.Wai.Handler.Warp
import UserService
import CryptoService
import Websocket

port :: Int
port = 8080

main :: IO ()
main = do
  let cryptoSvc = mkFakeCryptoService
  userSvc <- mkInMemoryUserService cryptoSvc
  wsApp <- mkWebsocketApp

  putStrLn $ "warp " <> warpVersion
  putStrLn $ "Starting server listening on port " <> (show port)

  run port (app userSvc wsApp)
