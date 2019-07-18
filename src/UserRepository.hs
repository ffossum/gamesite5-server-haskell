module UserRepository where

import Core.User (User(..), UserId, Username, Email)
import qualified Core.User as User
import Core.Password (PasswordHash)
import Control.Concurrent.STM
import Data.List (find)

data AddUser = AddUser Username Email PasswordHash

data AddUserErr = EmailAlreadyExists

data UserRepo m = UserRepo
  { getUser :: UserId -> m (Maybe User)
  , addNewUser :: AddUser -> m (Either AddUserErr User)
  }

mkInMemoryUserRepo :: IO (UserRepo IO)
mkInMemoryUserRepo = atomically $ do
    idCounterVar <- newTVar (0 :: Int)
    usersVar <- newTVar ([] :: [User])

    let
      _getUser :: UserId -> IO (Maybe User)
      _getUser userId = find (\user -> (User.userId user) == userId) <$> readTVarIO usersVar

      _addNewUser :: AddUser -> IO (Either AddUserErr User)
      _addNewUser (AddUser name email passwordHash) = atomically $ do
        users <- readTVar usersVar
        let existingEmail = find (\user -> (User.userEmail user) == email) users

        case existingEmail of
          Just _ ->
            pure $ Left EmailAlreadyExists

          Nothing -> do
            userId <- User.UserId <$> readTVar idCounterVar
            modifyTVar' idCounterVar (+1)
            let user = User
                  { User.userId = userId
                  , User.userName = name
                  , User.userEmail = email
                  , User.userPasswordHash = passwordHash
                  }
            modifyTVar' usersVar (user:)
            pure $ Right user

      in pure $ UserRepo _getUser _addNewUser
