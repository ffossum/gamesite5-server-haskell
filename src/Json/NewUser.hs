{-# LANGUAGE DeriveGeneric #-}

module Json.NewUser where

import Data.Text (Text)
import GHC.Generics
import Data.Aeson.Types

data NewUser = NewUser
  { name :: Text
  , email :: Text
  , password :: Text
  } deriving (Eq, Show, Generic)

instance FromJSON NewUser
