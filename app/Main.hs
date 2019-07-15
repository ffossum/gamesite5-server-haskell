module Main where

import Lib
import Server (app)
import Network.Wai.Handler.Warp

port :: Int
port = 8080

main :: IO ()
main = do
  putStrLn $ "Starting server listening on port " <> (show port)
  run port app
