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

spaceOnBoard :: Board -> Position -> Space
spaceOnBoard board position = board.grid ! position

buildUp :: Board -> Position -> Either BoardError Board
buildUp board targetPosition = do
  let targetSpace = spaceOnBoard board targetPosition
  let updatedGrid = insert targetPosition (targetSpace { level = succ targetSpace.level }) board.grid
  _ <- spaceHasNoWorker board targetSpace
  _ <- spaceCanBuildUp board targetSpace
  Right board { grid = updatedGrid }

placeWorker :: Board -> Worker -> Position -> Either BoardError Board
placeWorker board workerToPlace targetPosition = do
  let targetSpace = spaceOnBoard board targetPosition
  let updatedGrid = insert targetPosition (targetSpace { worker = JustWorker workerToPlace }) board.grid
  let updatedWorkers = insert workerToPlace targetPosition board.workers
  _ <- workerCanBePlaced board workerToPlace
  _ <- spaceHasNoWorker board targetSpace
  Right $ board { grid = updatedGrid, workers = updatedWorkers }

moveWorker :: Board -> Worker -> Position -> Either BoardError Board
moveWorker board workerToMove targetPosition = do
  let targetSpace = spaceOnBoard board targetPosition
  let originPosition = board.workers ! workerToMove
  let originSpace = spaceOnBoard board originPosition
  let updatedOriginSpace = (originPosition, originSpace { worker = NoWorker })
  let updatedTargetSpace = (targetPosition, targetSpace { worker = JustWorker workerToMove })
  let updatedGrid = insertMany [updatedOriginSpace, updatedTargetSpace] board.grid
  let updatedWorkers = insert workerToMove targetPosition board.workers
  _ <- spaceHasNoWorker board targetSpace
  _ <- spaceCanBeMovedInto board targetSpace
  Right $ board { grid = updatedGrid, workers = updatedWorkers }

spaceHasNoWorker :: Board -> Space -> Either BoardError Board
spaceHasNoWorker board space = case space.worker of
                                 JustWorker _   -> Left $ OccupiedError "Worker exists in this space"
                                 NoWorker       -> Right board

spaceCanBuildUp :: Board -> Space -> Either BoardError Board
spaceCanBuildUp board space = case space.level of
                                Dome      -> Left $ BuildError "Can't build on top of a dome"
                                otherwise -> Right board

spaceCanBeMovedInto :: Board -> Space -> Either BoardError Board
spaceCanBeMovedInto = spaceCanBuildUp

workerCanBePlaced :: Board -> Worker -> Either BoardError Board
workerCanBePlaced board worker = case board.workers ! worker of
                                   Position _ -> Left $ AlreadyPlacedWorkerError "Can't placed worker that's already on the board"
                                   NotOnBoard -> Right board
