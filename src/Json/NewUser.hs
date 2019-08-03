{-# LANGUAGE DeriveGeneric #-}

module Json.NewUser where

import Data.Aeson.Types
import Data.Text (Text)
import GHC.Generics

data NewUser
  = NewUser
      { name :: Text
      , email :: Text
      , password :: Text
      }
  deriving (Eq, Show, Generic)

instance FromJSON NewUser
