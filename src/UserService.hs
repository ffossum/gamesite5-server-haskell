module UserService where

import Control.Concurrent.STM
import Core.Password (Password, PasswordHash)
import Core.User (Email, User (..), UserId, Username)
import qualified Core.User as User
import CryptoService (CryptoService)
import qualified CryptoService as Crypto
import Data.List (find)

data AddUser = AddUser Username Email Password

data AddUserErr = EmailAlreadyExists

data UserService m
  = UserService
      { getUser :: UserId -> m (Maybe User)
      , getUserByEmail :: Email -> m (Maybe User)
      , getUsers :: [UserId] -> m [User]
      , addNewUser :: AddUser -> m (Either AddUserErr User)
      , validatePassword :: Password -> User -> Bool
      }

mkInMemoryUserService :: CryptoService -> IO (UserService IO)
mkInMemoryUserService cryptoSvc = do
  idCounterVar <- newTVarIO (0 :: Int)
  usersVar <- newTVarIO ([] :: [User])
  let _getUser :: UserId -> IO (Maybe User)
      _getUser userId = find (\user -> (User.userId user) == userId) <$> readTVarIO usersVar
      _getUserByEmail :: Email -> IO (Maybe User)
      _getUserByEmail email = find (\user -> (User.userEmail user) == email) <$> readTVarIO usersVar
      _getUsers :: [UserId] -> IO [User]
      _getUsers userIds = do
        users <- readTVarIO usersVar
        pure $ filter (\user -> (User.userId user) `elem` userIds) users
      _addNewUser :: AddUser -> IO (Either AddUserErr User)
      _addNewUser (AddUser name email password) =
        atomically $ do
          users <- readTVar usersVar
          let existingEmail = find (\user -> (User.userEmail user) == email) users
          case existingEmail of
            Just _ ->
              pure $ Left EmailAlreadyExists
            Nothing -> do
              userId <- User.UserId <$> readTVar idCounterVar
              modifyTVar' idCounterVar (+ 1)
              let passwordHash = Crypto.hashPassword cryptoSvc password
                  user = User
                    { User.userId = userId
                    , User.userName = name
                    , User.userEmail = email
                    , User.userPasswordHash = passwordHash
                    }
              modifyTVar' usersVar (user :)
              pure $ Right user
      _validatePassword :: Password -> User -> Bool
      _validatePassword password user =
        Crypto.validatePassword cryptoSvc password (User.userPasswordHash user)
   in pure $ UserService _getUser _getUserByEmail _getUsers _addNewUser _validatePassword
