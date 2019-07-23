{-# LANGUAGE OverloadedStrings #-}

module AuthCookie where

import qualified Web.Cookie as Cookie
import Core.User (UserId(..))
import Network.Wai (Request, requestHeaders)
import Parser.ByteString.Int (utf8IntMaybe)

userId :: Request -> Maybe (UserId)
userId req =
  let
    headers = requestHeaders req
  in
    do
      cookies <- Cookie.parseCookies <$> lookup "cookie" headers
      cookieValue <- lookup "name" cookies
      UserId <$> utf8IntMaybe cookieValue
