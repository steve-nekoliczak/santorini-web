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
  , BoardError (..)
  , emptyBoard
  , spaceOnBoard
  , buildUp
  , placeWorker
  , moveWorker
  ) where

import Prelude hiding (lookup)
import Data.Map hiding (map)
import Lib

data BoardError = BuildError String
                | MoveError String
                | OccupiedError String
                | AlreadyPlacedWorkerError String
                deriving (Show, Eq)

data XCoord = XA | XB | XC | XD | XE deriving (Show, Eq, Ord, Enum)
data YCoord = Y1 | Y2 | Y3 | Y4 | Y5 deriving (Show, Eq, Ord, Enum)
data Position = NotOnBoard | Position (XCoord, YCoord) deriving (Show, Eq, Ord)

data Level = Ground | LevelOne | LevelTwo | LevelThree | Dome deriving (Show, Eq, Ord, Enum, Bounded)

data Worker = BlueMan | BlueWoman | IvoryMan | IvoryWoman deriving (Show, Eq, Ord)
data MaybeWorker w = NoWorker | JustWorker w deriving (Show, Eq, Ord)

data Space = Space { level :: Level
                   , worker :: MaybeWorker Worker
                   } deriving (Show, Eq)

data Board = Board { grid :: (Map Position Space)
                   , workers :: (Map Worker Position)
                   } deriving (Show, Eq)

emptyBoard :: Board 
emptyBoard = Board grid workers
  where grid = fromList [(Position (x, y), Space Ground NoWorker) | x <- [XA .. XE], y <- [Y1 .. Y5]]
        workers = fromList [(BlueMan, NotOnBoard), (BlueWoman, NotOnBoard), (IvoryMan, NotOnBoard), (IvoryWoman, NotOnBoard)]

spaceOnBoard :: Position -> Board -> Space
spaceOnBoard position board = board.grid ! position

buildUp :: Position -> Board -> Either BoardError Board
buildUp targetPosition board =
  spaceHasNoWorker targetSpace board
  >> spaceCanBuildUp targetSpace board
  >> Right (board { grid = updatedGrid })
  where targetSpace = spaceOnBoard targetPosition board
        updatedGrid = insert targetPosition (targetSpace { level = succ targetSpace.level }) board.grid

placeWorker :: Worker -> Position -> Board -> Either BoardError Board
placeWorker workerToPlace targetPosition board =
  workerCanBePlaced workerToPlace board
  >> spaceHasNoWorker targetSpace board
  >> Right (board { grid = updatedGrid, workers = updatedWorkers })
  where targetSpace = spaceOnBoard targetPosition board
        updatedGrid = insert targetPosition (targetSpace { worker = JustWorker workerToPlace }) board.grid
        updatedWorkers = insert workerToPlace targetPosition board.workers

moveWorker :: Worker -> Position -> Board -> Either BoardError Board
moveWorker workerToMove targetPosition board =
  spaceHasNoWorker targetSpace board
  >> spaceCanBeMovedInto targetSpace board
  >> Right (board { grid = updatedGrid, workers = updatedWorkers })
  where targetSpace = spaceOnBoard targetPosition board
        originPosition = board.workers ! workerToMove
        originSpace = spaceOnBoard originPosition board
        updatedOriginSpace = (originPosition, originSpace { worker = NoWorker })
        updatedTargetSpace = (targetPosition, targetSpace { worker = JustWorker workerToMove })
        updatedGrid = insertMany [updatedOriginSpace, updatedTargetSpace] board.grid
        updatedWorkers = insert workerToMove targetPosition board.workers

spaceHasNoWorker :: Space -> Board -> Either BoardError Board
spaceHasNoWorker space board = case space.worker of
                                 JustWorker _   -> Left $ OccupiedError "Worker exists in this space"
                                 NoWorker       -> Right board

spaceCanBuildUp :: Space -> Board -> Either BoardError Board
spaceCanBuildUp space board = case space.level of
                                Dome      -> Left $ BuildError "Can't build on top of a dome"
                                _         -> Right board

spaceCanBeMovedInto :: Space -> Board -> Either BoardError Board
spaceCanBeMovedInto = spaceCanBuildUp

workerCanBePlaced :: Worker -> Board -> Either BoardError Board
workerCanBePlaced worker board = case board.workers ! worker of
                                   Position _ -> Left $ AlreadyPlacedWorkerError "Can't placed worker that's already on the board"
                                   NotOnBoard -> Right board
