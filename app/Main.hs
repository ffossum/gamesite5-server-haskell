module Main where

import Lib
import Server (app)
import Network.Wai.Handler.Warp
import UserService (mkInMemoryUserService)
import CryptoService (mkFakeCryptoService)
import WebSocket.App (mkWebsocketApp)

port :: Int
port = 8080

main :: IO ()
main = do
  let cryptoSvc = mkFakeCryptoService
  userSvc <- mkInMemoryUserService cryptoSvc
  wsApp <- mkWebsocketApp userSvc

  putStrLn $ "warp " <> warpVersion
  putStrLn $ "Starting server listening on port " <> (show port)

  run port (app userSvc wsApp)
