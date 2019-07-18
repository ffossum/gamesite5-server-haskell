{-# LANGUAGE DeriveGeneric #-}

module Json.User where

import GHC.Generics
import Data.Aeson.Types
import Data.Text (Text)

data User = User
  { id :: Int
  , name :: Text
  , email :: Text
  } deriving (Eq, Show, Generic)

instance ToJSON User