module Parser.ByteString.Int where

import Data.Attoparsec.Text
import Data.Bifunctor (first)
import Data.ByteString (ByteString)
import Data.Text.Encoding (decodeUtf8')

utf8Int :: Integral a => ByteString -> Either String a
utf8Int bs = do
  utf8 <- first show (decodeUtf8' bs)
  parseOnly (decimal <* endOfInput) utf8

utf8IntMaybe :: Integral a => ByteString -> Maybe a
utf8IntMaybe = eitherToMaybe . utf8Int

eitherToMaybe :: Either e a -> Maybe a
eitherToMaybe (Left _) = Nothing
eitherToMaybe (Right a) = Just a
