
module WebSocket.Event where

import Core.User

import Data.Text (Text)
import Data.Aeson
import Json.UserJson (UserJson)
import qualified Json.UserJson as UserJson

data EventJson = LoginSuccess UserJson

instance ToJSON EventJson where
  toJSON (LoginSuccess userJson) = toJSON [toJSON "login_success", toJSON userJson]

loginSuccessEvent :: User -> EventJson
loginSuccessEvent = LoginSuccess . UserJson.fromUser
