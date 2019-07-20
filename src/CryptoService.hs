module CryptoService where

import Core.Password (hash, Password(..), PasswordHash(..))
import Core.User (Email)
import qualified Crypto.KDF.BCrypt as BCrypt

data CryptoService = CryptoService
  { hashPassword :: Password -> PasswordHash
  }

validatePassword :: CryptoService -> Password -> PasswordHash -> Bool
validatePassword authService password passwordHash =
  hashPassword authService password == passwordHash

mkFakeCryptoService :: CryptoService
mkFakeCryptoService = CryptoService hash