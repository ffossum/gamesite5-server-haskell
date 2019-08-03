{-# LANGUAGE DeriveGeneric #-}

module Json.LoginJson where

import Data.Aeson.Types
import Data.Text (Text)
import GHC.Generics

data LoginJson
  = LoginJson
      { email :: Text
      , password :: Text
      }
  deriving (Eq, Show, Generic)

instance FromJSON LoginJson
