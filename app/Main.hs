module Main where

import Lib
import Server (app)
import Network.Wai.Handler.Warp
import UserRepository

port :: Int
port = 8080

main :: IO ()
main = do
  userRepo <- mkInMemoryUserRepo
  putStrLn $ "Starting server listening on port " <> (show port)
  run port (app userRepo)
