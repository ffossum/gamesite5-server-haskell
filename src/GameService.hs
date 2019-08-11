module GameService where

import Control.Concurrent.STM
import Core.User (UserId)
import qualified Core.User as User
import Data.List (find)
import UserService (UserService)
import qualified UserService

newtype GameId = GameId Int
  deriving (Eq, Show)

data Game
  = Game
      { gameId :: GameId
      , gameHost :: UserId
      , gamePlayers :: [UserId]
      }
  deriving (Eq, Show)

gameHostAndPlayers :: Game -> [UserId]
gameHostAndPlayers game = gameHost game : gamePlayers game

type Host = UserId

data JoinGameError = JoinGameNotFound | AlreadyJoined

data LeaveGameError = LeaveGameNotFound | NotInGame

data GameService m
  = GameService
      { createGame :: Host -> m Game
      , joinGame :: UserId -> GameId -> m (Either JoinGameError Game)
      , leaveGame :: UserId -> GameId -> m (Either LeaveGameError Game)
      , gameById :: GameId -> m (Maybe Game)
      , gamesByHost :: UserId -> m [Game]
      , gamesByUser :: UserId -> m [Game]
      }

mkInMemoryGameService :: IO (GameService IO)
mkInMemoryGameService = do
  idCounterVar <- newTVarIO (0 :: Int)
  gamesVar <- newTVarIO ([] :: [Game])
  let _createGame :: Host -> IO Game
      _createGame host =
        atomically $ do
          gid <- GameId <$> readTVar idCounterVar
          modifyTVar' idCounterVar (+ 1)
          let game = Game
                { gameId = gid
                , gameHost = host
                , gamePlayers = []
                }
          modifyTVar' gamesVar (game :)
          pure game
      _joinGame :: UserId -> GameId -> m (Either JoinGameError Game)
      _joinGame = undefined
      _leaveGame :: UserId -> GameId -> m (Either LeaveGameError Game)
      _leaveGame = undefined
      _gameById :: GameId -> IO (Maybe Game)
      _gameById gid =
        find (\game -> gameId game == gid) <$> readTVarIO gamesVar
      _gamesByHost :: Host -> IO [Game]
      _gamesByHost host =
        filter (\game -> gameHost game == host) <$> readTVarIO gamesVar
      _gamesByUser :: UserId -> IO [Game]
      _gamesByUser usrId =
        filter (\game -> usrId `elem` gameHostAndPlayers game) <$> readTVarIO gamesVar
   in pure $ GameService _createGame _joinGame _leaveGame _gameById _gamesByHost _gamesByUser
