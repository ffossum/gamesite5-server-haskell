{-# LANGUAGE OverloadedStrings #-}

module AuthCookie where

import Core.User (UserId (..))
import Network.HTTP.Types.Header (RequestHeaders)
import Parser.ByteString.Int (utf8IntMaybe)
import qualified Web.Cookie as Cookie

userId :: RequestHeaders -> Maybe (UserId)
userId headers = do
  cookies <- Cookie.parseCookies <$> lookup "cookie" headers
  cookieValue <- lookup "name" cookies
  UserId <$> utf8IntMaybe cookieValue
