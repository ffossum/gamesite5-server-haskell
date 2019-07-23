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
import Json.LoginJson (LoginJson(..))
import qualified Json.LoginJson as LoginJson
import Json.NewUser (NewUser(..))
import qualified Json.NewUser as NewUser
import Json.PublicUserJson (PublicUserJson(..))
import qualified Json.PublicUserJson as PublicUserJson
import Servant.HTML.Lucid
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Encoding (encodeUtf8)
import qualified Html.MainPage as MainPage
import Lucid (Html)
import qualified Web.Cookie as Cookie
import Network.Wai (Request, requestHeaders)
import Servant.Server.Experimental.Auth (AuthHandler, AuthServerData, mkAuthHandler)
import UserService (UserService, AddUser(..))
import qualified UserService as UserService
import Core.Password (hash, Password(..))
import Core.User (UserId(..), Username(..), Email(..))
import qualified Core.User as User
import Control.Monad.Trans (liftIO)
import Data.Time (UTCTime(..), secondsToDiffTime)
import Data.Time.Calendar (fromGregorian)
import Parser.ByteString.Int (utf8IntMaybe)
import qualified AuthCookie

type UserApi =
  "users" :>
    ( "me" :> AuthProtect "cookie-required" :> Get '[JSON] UserJson
    )
  :<|> "register" :> ReqBody '[JSON] NewUser :> PostCreated '[JSON] (WithCookie UserJson)
  :<|> "login" :> ReqBody '[JSON] LoginJson :> PostAccepted '[JSON] (WithCookie UserJson)
  :<|> "logout" :> GetNoContent '[JSON] (WithCookie NoContent)


type instance AuthServerData (AuthProtect "cookie-required") = UserId
type instance AuthServerData (AuthProtect "cookie-optional") = Maybe UserId

type RestApi = "api" :> UserApi

type WebsocketApi = "ws" :> Raw

type HtmlApi = CaptureAll "segments" Text :> Get '[HTML] (Html ())

type Api = RestApi :<|> WebsocketApi :<|> HtmlApi

type WithCookie a = Headers '[Header "Set-Cookie" Cookie.SetCookie] a


baseCookie :: Cookie.SetCookie
baseCookie = Cookie.defaultSetCookie
  { Cookie.setCookieHttpOnly = True
  , Cookie.setCookiePath = Just "/"
  }

createCookie :: UserId -> Cookie.SetCookie
createCookie (UserId userId) = baseCookie
  { Cookie.setCookieValue = encodeUtf8 (Text.pack (show userId))
  }

createExpiredCookie :: Cookie.SetCookie
createExpiredCookie = baseCookie
  { Cookie.setCookieExpires = Just (UTCTime (fromGregorian 1970 1 1) (secondsToDiffTime 0))
  , Cookie.setCookieValue = "expired"
  }

restServer :: UserService IO -> Server RestApi
restServer userSvc =
  meEndpoint :<|> registerEndpoint :<|> loginEndpoint :<|> logoutEndpoint
    where
      meEndpoint :: UserId -> Handler UserJson
      meEndpoint userId = do
        maybeUser <- liftIO $ UserService.getUser userSvc userId
        case maybeUser of
          Just user -> pure (UserJson.fromUser user)
          Nothing -> throwError err404 { errBody = "user with id not found" }

      registerEndpoint :: NewUser -> Handler (WithCookie UserJson)
      registerEndpoint newUser = do
        let
          user = AddUser (Username (NewUser.name newUser)) (Email (NewUser.email newUser)) (Password (NewUser.password newUser))
        addResult <- liftIO $ UserService.addNewUser userSvc user
        case addResult of
          Left UserService.EmailAlreadyExists -> throwError err403 { errBody = "user with email already exists" }
          Right addedUser -> pure $ addHeader (createCookie (User.userId addedUser)) (UserJson.fromUser addedUser)

      loginEndpoint :: LoginJson -> Handler (WithCookie UserJson)
      loginEndpoint loginJson = do
        let
          email = Email (LoginJson.email loginJson)
          password = Password (LoginJson.password loginJson)

        maybeUser <- liftIO $ UserService.getUserByEmail userSvc email
        let maybeValid = UserService.validatePassword userSvc password <$> maybeUser

        case (maybeUser, maybeValid) of
          (Just user, Just True) -> pure $ addHeader (createCookie (User.userId user)) (UserJson.fromUser user)
          _ -> throwError err403 { errBody = "login failed" }

      logoutEndpoint :: Handler (WithCookie NoContent)
      logoutEndpoint = pure $ addHeader createExpiredCookie NoContent

wsServer :: Application -> Server WebsocketApi
wsServer wsApp = pure wsApp

htmlServer :: Server HtmlApi
htmlServer segments = pure MainPage.html

server :: UserService IO -> Application ->  Server Api
server userSvc wsApp = (restServer userSvc) :<|> (wsServer wsApp) :<|> htmlServer

apiProxy :: Proxy Api
apiProxy = Proxy

app :: UserService IO -> Application -> Application
app userSvc wsApp = serveWithContext apiProxy context (server userSvc wsApp)
  where
    context = authRequiredHandler :. authOptionalHandler :. EmptyContext

authRequiredHandler :: AuthHandler Request UserId
authRequiredHandler = mkAuthHandler handler
  where
    handler :: Request -> Handler UserId
    handler req =
        case AuthCookie.userId (requestHeaders req) of
          Just userId -> pure userId
          Nothing -> throwError $ err401

authOptionalHandler :: AuthHandler Request (Maybe UserId)
authOptionalHandler = mkAuthHandler (pure . AuthCookie.userId . requestHeaders)
