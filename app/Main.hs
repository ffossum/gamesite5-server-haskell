module Main where

import Lib
import Server (app)
import Network.Wai.Handler.Warp
import Network.Wai.Middleware.Cors

port :: Int
port = 8080

corsMiddleware =
  let policy = simpleCorsResourcePolicy { corsRequestHeaders = simpleHeaders }
  in  cors (const (Just policy))

main :: IO ()
main = do
  putStrLn $ "Starting server listening on port " <> (show port)
  run port (corsMiddleware app)
