module Gameplay
  (
  ) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.State
import Engine
import Data.Either

data Player = BluePlayer | IvoryPlayer deriving (Show, Eq)

data GameState = PlaceWorkers
               | BluePlayerTurn
               | IvoryPlayerTurn
               | GameOver
               deriving (Show, Eq)

gameplayLoopT :: Board -> StateT GameState IO (Either BoardError Board)
gameplayLoopT board = do
  liftIO $ putStrLn $ show board

  currState <- get

  newBoardT <-
        case currState of
          PlaceWorkers    -> placeWorkerT board

  case newBoardT of
    Left errorMessage -> liftIO $ print errorMessage
    Right newBoard    ->
      case currState of
        PlaceWorkers    ->
          case nextWorkerToPlace newBoard of
            NoWorker      -> put BluePlayerTurn
            JustWorker _  -> put PlaceWorkers

  newState <- get

  case newBoardT of
    Left _              -> gameplayLoopT board
    Right newBoard      ->
      case newState of
        GameOver        -> return newBoardT
        BluePlayerTurn  -> return newBoardT -- TODO: Change this once turns are implemented.
        _               -> gameplayLoopT newBoard

main :: IO ()
main = do
  putStrLn "Starting new game of Santorini!"
  let startGame = gameplayLoopT emptyBoard
  let initialState = PlaceWorkers
  newGame <- runStateT startGame initialState
  putStrLn $ show $ fst newGame
  return ()

placeWorkerT :: Board -> StateT GameState IO (Either BoardError Board)
placeWorkerT board = do
  input <- case nextWorkerToPlace board of
    JustWorker worker -> do
      liftIO $ print $ "Please place " ++ (show worker) ++ " character"
      liftIO $ getLine
    -- NoWorker -> do
    --   TODO: Add exception here

  let targetPosition = read input :: Position
  let newBoard = placeNextWorker targetPosition board

  return newBoard
