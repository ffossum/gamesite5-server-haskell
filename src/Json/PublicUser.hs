{-# LANGUAGE DeriveGeneric #-}

module Json.PublicUser (PublicUser(..), fromUser) where

import qualified Json.User as User
import GHC.Generics
import Data.Aeson.Types

data PublicUser = PublicUser
  { id :: Int
  , name :: String
  } deriving (Eq, Show, Generic)

instance ToJSON PublicUser

fromUser :: User.User -> PublicUser
fromUser user = PublicUser (User.id user) (User.name user)
