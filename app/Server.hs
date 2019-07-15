{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Server where

import GHC.Generics
import Servant
import Data.Aeson.Types
import qualified Gamesite.Json.User as User
import qualified Gamesite.Json.NewUser as NewUser
import Servant.HTML.Lucid
import Data.Text (Text)

exampleUser :: User.User
exampleUser = User.User
  { User.id = 1
  , User.name = "John"
  , User.email = "john@test.test"
  }

type UserApi =
  "users" :>
    ("me" :> Get '[JSON] User.User
    :<|> ReqBody '[JSON] NewUser.NewUser :> PostCreated '[JSON] User.User
    )

type RestApi = "api" :> UserApi

type HtmlApi = CaptureAll "segments" Text :> Get '[HTML] Text

type Api = RestApi :<|> HtmlApi


getUser :: Handler User.User
getUser = pure exampleUser

createUser :: NewUser.NewUser -> Handler User.User
createUser newUser =
  pure User.User
    { User.id = 2
    , User.email = NewUser.email newUser
    , User.name = NewUser.name newUser
    }

restServer :: Server RestApi
restServer = (getUser  :<|> createUser)

htmlServer :: Server HtmlApi
htmlServer segments = pure "hello"

server :: Server Api
server = restServer :<|> htmlServer

apiProxy :: Proxy Api
apiProxy = Proxy

app :: Application
app = serve apiProxy server

