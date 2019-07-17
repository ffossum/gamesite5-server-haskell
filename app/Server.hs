{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
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
import Json.PublicUser (PublicUser(..))
import qualified Json.PublicUser as PublicUser
import Servant.HTML.Lucid
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Html.MainPage as MainPage
import Lucid (Html)
import qualified Web.Cookie as Cookie
import Network.Wai                      (Request, requestHeaders)
import Servant.Server.Experimental.Auth (AuthHandler, AuthServerData, mkAuthHandler)


exampleUser :: User
exampleUser = User
  { User.id = 1
  , User.name = "John"
  , User.email = "john@test.test"
  }

type UserApi =
  "users" :>
    ("me" :> AuthProtect "cookie-auth" :> Get '[JSON] User
    :<|> ReqBody '[JSON] NewUser :> PostCreated '[JSON] (WithCookie User)
    )

type instance AuthServerData (AuthProtect "cookie-auth") = PublicUser

type RestApi = "api" :> UserApi

type HtmlApi = CaptureAll "segments" Text :> Get '[HTML] (Html ())

type Api = RestApi :<|> HtmlApi

type WithCookie a = Headers '[Header "Set-Cookie" Cookie.SetCookie] a

createCookie :: PublicUser -> Cookie.SetCookie
createCookie user =
  Cookie.defaultSetCookie
    { Cookie.setCookieHttpOnly = True
    , Cookie.setCookiePath = Just "/"
    }

restServer :: Server RestApi
restServer = getUser :<|> createUser
    where
      getUser :: PublicUser -> Handler User
      getUser publicUser = pure exampleUser

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

htmlServer :: Server HtmlApi
htmlServer segments = pure MainPage.html

server :: Server Api
server = restServer :<|> htmlServer

apiProxy :: Proxy Api
apiProxy = Proxy

app :: Application
app = serveWithContext apiProxy context server
  where
    context = authHandler :. EmptyContext

authHandler :: AuthHandler Request PublicUser
authHandler = mkAuthHandler handler
  where
    handler :: Request -> Handler PublicUser
    handler req =
      let
        headers = requestHeaders req
        token = do
          cookies <- Cookie.parseCookies <$> lookup "cookie" headers
          lookup "name" cookies
      in
        case token of
          Just _ -> pure (PublicUser 1 "asdf")
          Nothing -> throwError $ err401

