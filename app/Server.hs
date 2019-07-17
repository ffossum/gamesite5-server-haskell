{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Server where

import GHC.Generics
import Servant
import Data.Aeson.Types
import Json.User (User(..))
import qualified Json.User as User
import Json.NewUser (NewUser(..))
import qualified Json.NewUser as NewUser
import Json.PublicUser (PublicUser)
import qualified Json.PublicUser as PublicUser
import Servant.HTML.Lucid
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Html.MainPage as MainPage
import Lucid (Html)
import qualified Web.Cookie as Cookie

exampleUser :: User
exampleUser = User
  { User.id = 1
  , User.name = "John"
  , User.email = "john@test.test"
  }

type UserApi =
  "users" :>
    ("me" :> Get '[JSON] User
    :<|> ReqBody '[JSON] NewUser :> PostCreated '[JSON] (WithCookie User)
    )

type RestApi = "api" :> UserApi

type HtmlApi = CaptureAll "segments" Text :> Get '[HTML] (Html ())

type Api = RestApi :<|> HtmlApi

type WithCookie a = Headers '[Header "Set-Cookie" Cookie.SetCookie] a

getUser :: Handler User
getUser = pure exampleUser

createCookie :: PublicUser -> Cookie.SetCookie
createCookie user =
  Cookie.defaultSetCookie
    { Cookie.setCookieHttpOnly = True
    , Cookie.setCookiePath = Just "/"
    }

createUser :: NewUser -> Handler (WithCookie User)
createUser newUser =
  let
    user = User
      { User.id = 2
      , User.email = NewUser.email newUser
      , User.name = NewUser.name newUser
      }
  in
    pure $ addHeader (createCookie (PublicUser.fromUser user)) user

restServer :: Server RestApi
restServer = getUser  :<|> createUser

htmlServer :: Server HtmlApi
htmlServer segments = pure MainPage.html

server :: Server Api
server = restServer :<|> htmlServer

apiProxy :: Proxy Api
apiProxy = Proxy

app :: Application
app = serve apiProxy server

