module Core.Password (Password(..), PasswordHash, unPasswordHash, hash) where

import Data.Text (Text)

newtype Password = Password { unPassword :: Text } deriving (Eq, Show)
newtype PasswordHash = PasswordHash { unPasswordHash :: Text } deriving (Eq, Show)

hash :: Password -> PasswordHash
hash = PasswordHash . unPassword -- TODO
