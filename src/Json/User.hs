{-# LANGUAGE DeriveGeneric #-}

module Json.User where

import GHC.Generics
import Data.Aeson.Types

data User = User
  { id :: Int
  , name :: String
  , email :: String
  } deriving (Eq, Show, Generic)

instance ToJSON User