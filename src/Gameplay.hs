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
  let newBoard = placeWorker BlueMan (Position (XA, Y1)) board
  return newBoard

main :: IO ()
main = do
  putStrLn "Starting new game of Santorini!"
  let startGame = gameplayLoopT emptyBoard
  let initialState = PlaceWorkers
  newGame <- runStateT startGame initialState
  putStrLn $ show $ fst newGame
  return ()
