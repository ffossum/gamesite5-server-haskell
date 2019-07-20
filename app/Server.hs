{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Server where

import GHC.Generics
import Servant
import Data.Aeson.Types
import Json.UserJson (UserJson(..))
import qualified Json.UserJson as UserJson
import Json.NewUser (NewUser(..))
import qualified Json.NewUser as NewUser
import Json.PublicUserJson (PublicUserJson(..))
import qualified Json.PublicUserJson as PublicUserJson
import Servant.HTML.Lucid
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Html.MainPage as MainPage
import Lucid (Html)
import qualified Web.Cookie as Cookie
import Network.Wai (Request, requestHeaders)
import Servant.Server.Experimental.Auth (AuthHandler, AuthServerData, mkAuthHandler)
import UserService (UserService, AddUser(..))
import qualified UserService as UserService
import Core.Password (hash, Password(..))
import Core.User
import Control.Monad.Trans (liftIO)

exampleUser :: UserJson
exampleUser = UserJson
  { UserJson.id = 1
  , UserJson.name = "John"
  , UserJson.email = "john@test.test"
  }

type UserApi =
  "users" :>
    ("me" :> AuthProtect "cookie-auth" :> Get '[JSON] UserJson
    :<|> ReqBody '[JSON] NewUser :> PostCreated '[JSON] (WithCookie UserJson)
    )

type instance AuthServerData (AuthProtect "cookie-auth") = PublicUserJson

type RestApi = "api" :> UserApi

type HtmlApi = CaptureAll "segments" Text :> Get '[HTML] (Html ())

type Api = RestApi :<|> HtmlApi

type WithCookie a = Headers '[Header "Set-Cookie" Cookie.SetCookie] a

createCookie :: PublicUserJson -> Cookie.SetCookie
createCookie user =
  Cookie.defaultSetCookie
    { Cookie.setCookieHttpOnly = True
    , Cookie.setCookiePath = Just "/"
    }

restServer :: UserService IO -> Server RestApi
restServer userSvc = getUser :<|> createUser
    where
      getUser :: PublicUserJson -> Handler UserJson
      getUser publicUser = do
        let i = UserId (PublicUserJson.id publicUser)
        maybeUser <- liftIO $ UserService.getUser userSvc i
        case maybeUser of
          Just user -> pure (UserJson.fromUser user)
          Nothing -> throwError err404 { errBody = "user with id not found" }

      createUser :: NewUser -> Handler (WithCookie UserJson)
      createUser newUser = do
        let
          user = AddUser (Username (NewUser.name newUser)) (Email (NewUser.email newUser)) (Password (NewUser.password newUser))
        addResult <- liftIO $ UserService.addNewUser userSvc user
        case addResult of
          Left UserService.EmailAlreadyExists -> throwError err403 { errBody = "user with email already exists" }
          Right addedUser -> pure $ addHeader (createCookie (PublicUserJson.fromUser addedUser)) (UserJson.fromUser addedUser)

htmlServer :: Server HtmlApi
htmlServer segments = pure MainPage.html

server :: UserService IO -> Server Api
server userSvc = (restServer userSvc) :<|> htmlServer

apiProxy :: Proxy Api
apiProxy = Proxy

app :: UserService IO -> Application
app userSvc = serveWithContext apiProxy context (server userSvc)
  where
    context = authHandler :. EmptyContext

authHandler :: AuthHandler Request PublicUserJson
authHandler = mkAuthHandler handler
  where
    handler :: Request -> Handler PublicUserJson
    handler req =
      let
        headers = requestHeaders req
        token = do
          cookies <- Cookie.parseCookies <$> lookup "cookie" headers
          lookup "name" cookies
      in
        case token of
          Just _ -> pure (PublicUserJson 1 "asdf")
          Nothing -> throwError $ err401

