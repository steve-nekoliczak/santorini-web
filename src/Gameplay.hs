module Gameplay
  (
  ) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.State
import Engine
import Data.Either

data GameState =
    PlaceWorkers
  | MoveWorker  { player :: Player }
  | BuildUp     { player :: Player, worker :: Worker }
  | GameOver
  deriving (Show, Eq)

type GameStateT = StateT GameState IO (Either BoardError Board)

main :: IO ()
main = do
  putStrLn "Starting new game of Santorini!"
  let startGame = gameplayLoopT emptyBoard
  let initialState = PlaceWorkers
  newGame <- runStateT startGame initialState
  putStrLn $ show $ fst newGame
  return ()

gameplayLoopT :: Board -> GameStateT
gameplayLoopT board = do
  liftIO $ putStrLn $ show board

  state <- get

  boardAfterAction <-
        case state of
          PlaceWorkers          -> placeNextWorkerT board
          MoveWorker player     -> moveWorkerT player board
          BuildUp player worker -> buildUpT player worker board
          GameOver              -> return $ Right board

  stateAfterAction <- get

  case boardAfterAction of
    Left _              -> gameplayLoopT board
    Right newBoard      ->
      case stateAfterAction of
        GameOver        -> return boardAfterAction
        _               -> gameplayLoopT newBoard

placeNextWorkerT :: Board -> GameStateT
placeNextWorkerT board = do
  targetPosition <- case nextWorkerToPlace board of
    Just worker -> do
      readPosition $ "Please place " ++ show worker ++ " character"
    -- Nothing -> do
    --   TODO: Add exception here

  let boardAfterAction = placeNextWorker targetPosition board

  case boardAfterAction of
    Left errorMessage -> liftIO $ print errorMessage
    Right newBoard    ->
      case nextWorkerToPlace newBoard of
        Nothing       -> put $ MoveWorker BluePlayer
        Just _        -> put PlaceWorkers

  return boardAfterAction

moveWorkerT :: Player -> Board -> GameStateT
moveWorkerT player board = do
  let workers = workersForPlayer player

  worker <- readWorker $ "Select a character for " ++ show player ++ ": " ++ show workers
  targetPosition <- readPosition $ "Select target position for " ++ show worker

  let boardAfterAction = moveWorker worker targetPosition board

  case boardAfterAction of
    Left errorMessage -> liftIO $ print errorMessage
    Right _           -> put $ BuildUp player worker

  return boardAfterAction

buildUpT :: Player -> Worker -> Board -> GameStateT
buildUpT player worker board = do
  targetPosition <- readPosition $ "Select target position to build for " ++ show worker

  let boardAfterAction = buildUp worker targetPosition board

  case boardAfterAction of
    Left errorMessage -> liftIO $ print errorMessage
    Right _           -> put $ MoveWorker $ nextPlayer player

  return boardAfterAction

readPosition :: String -> StateT GameState IO Position
readPosition message = do
  liftIO $ print message
  positionInput <- liftIO $ getLine
  let position = read positionInput :: Position

  return position

readWorker :: String -> StateT GameState IO Worker
readWorker message = do
  liftIO $ print message
  workerInput <- liftIO $ getLine
  let worker = read workerInput :: Worker

  return worker
