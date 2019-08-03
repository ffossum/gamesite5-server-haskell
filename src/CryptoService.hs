module CryptoService where

import Core.Password (Password (..), PasswordHash (..), hash)
import Core.User (Email)
import qualified Crypto.KDF.BCrypt as BCrypt

data CryptoService
  = CryptoService
      { hashPassword :: Password -> PasswordHash
      }

validatePassword :: CryptoService -> Password -> PasswordHash -> Bool
validatePassword authService password passwordHash =
  hashPassword authService password == passwordHash

mkFakeCryptoService :: CryptoService
mkFakeCryptoService = CryptoService hash
