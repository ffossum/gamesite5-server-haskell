module Core.User where

import Data.Text (Text)
import Core.Password

newtype UserId = UserId { unUserId :: Int } deriving (Eq, Show)
newtype Username = Username { unUsername :: Text } deriving (Eq, Show)
newtype Email = Email { unEmail :: Text } deriving (Eq, Show)

data User = User
  { userId :: UserId
  , userName :: Username
  , userEmail :: Email
  , userPasswordHash :: PasswordHash
  } deriving (Eq, Show)

