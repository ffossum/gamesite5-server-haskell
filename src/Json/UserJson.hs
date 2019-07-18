{-# LANGUAGE DeriveGeneric #-}

module Json.UserJson where

import GHC.Generics
import Data.Aeson.Types
import Data.Text (Text)
import Core.User

data UserJson = UserJson
  { id :: Int
  , name :: Text
  , email :: Text
  } deriving (Eq, Show, Generic)

instance ToJSON UserJson

fromUser :: User -> UserJson
fromUser user = UserJson
  { Json.UserJson.id = unUserId (userId user)
  , Json.UserJson.name = unUsername (userName user)
  , Json.UserJson.email = unEmail (userEmail user)
  }
