module Config where

import System.Environment

newtype PostgresHost = PostgresHost String

newtype RedisHost = RedisHost String

data Config = Config PostgresHost RedisHost

getConfig :: IO Config
getConfig = do
  redisHost <- RedisHost <$> getEnv "REDIS_HOST"
  postgresHost <- PostgresHost <$> getEnv "POSTGRES_HOST"
  pure $ Config postgresHost redisHost
