module Main where

import Lib
import Server (app)
import Network.Wai.Handler.Warp
import UserService
import CryptoService

port :: Int
port = 8080

main :: IO ()
main = do
  let cryptoSvc = mkFakeCryptoService
  userSvc <- mkInMemoryUserService cryptoSvc
  putStrLn $ "Starting server listening on port " <> (show port)
  run port (app userSvc)
