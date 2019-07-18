{-# LANGUAGE DeriveGeneric #-}

module Json.PublicUser (PublicUser(..), fromUser) where

import Json.User (User)
import qualified Json.User as User
import GHC.Generics
import Data.Aeson.Types
import Data.Text (Text)

data PublicUser = PublicUser
  { id :: Int
  , name :: Text
  } deriving (Eq, Show, Generic)

instance ToJSON PublicUser

fromUser :: User -> PublicUser
fromUser user = PublicUser
  { Json.PublicUser.id = User.id user
  , Json.PublicUser.name = User.name user
  }
