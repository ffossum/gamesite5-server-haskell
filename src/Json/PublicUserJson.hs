{-# LANGUAGE DeriveGeneric #-}

module Json.PublicUserJson
  ( PublicUserJson (..)
  , fromUser
  )
where

import Core.User (User)
import qualified Core.User as User
import Data.Aeson.Types
import Data.Text (Text)
import GHC.Generics

data PublicUserJson
  = PublicUserJson
      { id :: Int
      , name :: Text
      }
  deriving (Eq, Show, Generic)

instance ToJSON PublicUserJson

fromUser :: User -> PublicUserJson
fromUser user = PublicUserJson
  { Json.PublicUserJson.id = User.unUserId (User.userId user)
  , Json.PublicUserJson.name = User.unUsername (User.userName user)
  }
