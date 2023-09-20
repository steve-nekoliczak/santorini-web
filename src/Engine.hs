-- HACK: This is here to silence linter warnings.
{-# LANGUAGE OverloadedRecordDot #-}

module Engine
  ( XCoord (..)
  , YCoord (..)
  , Position (..)
  , Level (..)
  , Space (..)
  , Worker (..)
  , MaybeWorker (..)
  , Board (..)
  , emptyBoard
  , spaceOnBoard
  ) where

import Prelude hiding (lookup)
import Data.Map hiding (map)
import Lib

newtype BuildError = BuildError String deriving (Show)
newtype MoveError = MoveError String deriving (Show)

data XCoord = XA | XB | XC | XD | XE deriving (Show, Eq, Ord, Enum)
data YCoord = Y1 | Y2 | Y3 | Y4 | Y5 deriving (Show, Eq, Ord, Enum)
data Position = NotOnBoard | Position (XCoord, YCoord) deriving (Show, Eq, Ord)

data Top = Top
class Stackable a where
  step :: a -> Either Top a
  curr :: a -> Either Top a

data Level = Ground | LevelOne | LevelTwo | LevelThree | Dome deriving (Show, Eq)

instance Stackable Level where
  step Ground = Right LevelOne
  step LevelOne = Right LevelTwo
  step LevelTwo = Right LevelThree
  step LevelThree = Right Dome
  step Dome = Left Top
  curr Dome = Left Top
  curr level = Right level

data Worker = BlueMan | BlueWoman | IvoryMan | IvoryWoman deriving (Show, Eq, Ord)
data MaybeWorker w = NoWorker | JustWorker w deriving (Show, Eq, Ord)

data Space = Space { level :: Level
                   , worker :: MaybeWorker Worker
                   } deriving (Show, Eq)

data Board = Board { grid :: (Map Position Space)
                   , workers :: (Map Worker Position)
                   }  deriving (Show, Eq)

emptyBoard :: Board 
emptyBoard = Board grid workers
  where grid = fromList [(Position (x, y), Space Ground NoWorker) | x <- [XA .. XE], y <- [Y1 .. Y5]]
        workers = fromList [(BlueMan, NotOnBoard), (BlueWoman, NotOnBoard), (IvoryMan, NotOnBoard), (IvoryWoman, NotOnBoard)]

spaceOnBoard :: Board -> Position -> Space
spaceOnBoard (Board { grid }) position = grid ! position

buildUp :: Board -> Position -> Either BuildError Board 
buildUp board targetPosition =
  let targetSpace = spaceOnBoard board targetPosition
  in case targetSpace.worker of
    JustWorker _            -> Left $ BuildError "Can't build where a worker is"
    NoWorker            ->
      case step $ targetSpace.level of
        Left Top        -> Left $ BuildError "Can't build on top of a dome"
        Right newLevel  -> Right $ board { grid = updatedGrid }
          where updatedGrid = insert targetPosition (targetSpace { level = newLevel }) board.grid

moveWorker :: Board -> Worker -> Position -> Either MoveError Board
moveWorker board workerToMove targetPosition =
  let originPosition = board.workers ! workerToMove
      originSpace = spaceOnBoard board originPosition
      targetSpace = spaceOnBoard board targetPosition
  in case targetSpace.worker of
    JustWorker _        -> Left $ MoveError "Can't move where a worker is"
    NoWorker            ->
      case curr $ targetSpace.level of
        Left Top        -> Left $ MoveError "Can't move on top of a dome"
        Right _         -> Right $ board { grid = updatedGrid, workers = updatedWorkers }
          where updatedGrid = insertMany [updatedOriginSpace, updatedTargetSpace] board.grid
                updatedWorkers = insert workerToMove targetPosition board.workers
                updatedOriginSpace = (originPosition, originSpace { worker = NoWorker })
                updatedTargetSpace = (targetPosition, targetSpace { worker = JustWorker workerToMove })

placeWorker :: Board -> Worker -> Position -> Either MoveError Board
placeWorker board workerToPlace targetPosition =
  let targetSpace = spaceOnBoard board targetPosition
  in case board.workers ! workerToPlace of
    Position _          -> Left $ MoveError "Can't place worker that's already on the board"
    NotOnBoard          ->
      case targetSpace.worker of
        JustWorker _    -> Left $ MoveError "Can't place a worker where a worker is"
        NoWorker        -> Right $ board { grid = updatedGrid, workers = updatedWorkers }
      where updatedGrid = insert targetPosition (targetSpace { worker = JustWorker workerToPlace }) board.grid
            updatedWorkers = insert workerToPlace targetPosition board.workers
