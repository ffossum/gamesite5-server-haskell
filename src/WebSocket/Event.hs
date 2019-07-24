
module WebSocket.Event where

import Core.User

import Data.Text (Text)
import Data.Aeson
import Json.UserJson (UserJson)
import qualified Json.UserJson as UserJson

data EventJson = LoginEvent (Maybe UserJson)

instance ToJSON EventJson where
  toJSON (LoginEvent maybeUser) = toJSON [toJSON "login", toJSON maybeUser]

loginEvent :: (Maybe User) -> EventJson
loginEvent = LoginEvent . fmap UserJson.fromUser
