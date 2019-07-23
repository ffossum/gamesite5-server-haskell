{-# LANGUAGE OverloadedStrings #-}

module AuthCookie where

import qualified Web.Cookie as Cookie
import Core.User (UserId(..))
import Parser.ByteString.Int (utf8IntMaybe)
import Network.HTTP.Types.Header (RequestHeaders)

userId :: RequestHeaders -> Maybe (UserId)
userId headers = do
  cookies <- Cookie.parseCookies <$> lookup "cookie" headers
  cookieValue <- lookup "name" cookies
  UserId <$> utf8IntMaybe cookieValue
