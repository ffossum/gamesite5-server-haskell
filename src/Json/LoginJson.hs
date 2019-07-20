{-# LANGUAGE DeriveGeneric #-}

module Json.LoginJson where

import GHC.Generics
import Data.Aeson.Types
import Data.Text (Text)

data LoginJson = LoginJson
  { email :: Text
  , password :: Text
  } deriving (Eq, Show, Generic)

instance FromJSON LoginJson
