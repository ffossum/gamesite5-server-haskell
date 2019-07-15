{-# LANGUAGE DeriveGeneric #-}

module Json.NewUser where

import GHC.Generics
import Data.Aeson.Types

data NewUser = NewUser
  { name :: String
  , email :: String
  , password :: String
  } deriving (Eq, Show, Generic)

instance FromJSON NewUser
