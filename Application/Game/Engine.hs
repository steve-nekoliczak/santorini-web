-- HACK: This is here to silence linter warnings.
{-# LANGUAGE OverloadedRecordDot #-}

module Application.Game.Engine
  ( XCoord (..)
  , YCoord (..)
  , Position (..)
  , Level (..)
  , Space (..)
  , Player (..)
  , Worker (..)
  , Board (..)
  , BoardError (..)
  , emptyBoard
  , workersForPlayer
  , nextPlayer
  , spaceOnBoard
  , buildUp
  , nextWorkerToPlace
  , placeWorker
  , placeNextWorker
  , moveWorker
  , spaceIsAdjacent
  ) where

import Data.Map (Map, (!), fromList, insert)
import Data.Text (Text)
import Prelude

import Application.Helper.Map (insertMany)

data BoardError = BuildError String
                | MoveError String
                | OccupiedError String
                | AlreadyPlacedWorkerError String
                | WorkerNotYetPlacedError String
                | TargetSpaceNotAdjacentError String
                | AllWorkersPlacedError String
                | InvalidPositionError String
                deriving (Show, Eq)

data XCoord = XA | XB | XC | XD | XE deriving (Show, Eq, Ord, Enum)
instance Read XCoord where
  readsPrec _ = convertXCoord
convertXCoord :: String -> [(XCoord, String)]
convertXCoord "A" = [(XA, "")]
convertXCoord "B" = [(XB, "")]
convertXCoord "C" = [(XC, "")]
convertXCoord "D" = [(XD, "")]
convertXCoord "E" = [(XE, "")]
convertXCoord _ = error "Invalid X coordinate"

data YCoord = Y1 | Y2 | Y3 | Y4 | Y5 deriving (Show, Eq, Ord, Enum)
instance Read YCoord where
  readsPrec _ = convertYCoord
convertYCoord :: String -> [(YCoord, String)]
convertYCoord "1" = [(Y1, "")]
convertYCoord "2" = [(Y2, "")]
convertYCoord "3" = [(Y3, "")]
convertYCoord "4" = [(Y4, "")]
convertYCoord "5" = [(Y5, "")]
convertYCoord _ = error "Invalid Y coordinate"

data Position = NotOnBoard | Position (XCoord, YCoord) deriving (Show, Eq, Ord)
instance Read Position where
  readsPrec _ = convertPosition
convertPosition :: String -> [(Position, String)]
convertPosition [x, y] = [(Position (read [x], read [y]), "")]
convertPosition _ = error "Invalid Position"

data Level = Ground | LevelOne | LevelTwo | LevelThree | Dome deriving (Show, Eq, Ord, Enum, Bounded)

data Player = BluePlayer | IvoryPlayer deriving (Show, Eq)
data Worker = BlueMan | BlueWoman | IvoryMan | IvoryWoman deriving (Show, Read, Eq, Ord)

data Space = Space { level :: Level
                   , worker :: Maybe Worker
                   } deriving (Show, Eq)

data Board = Board { grid :: Map Position Space
                   , workers :: Map Worker Position
                   } deriving (Show, Eq)

emptyBoard :: Board 
emptyBoard = Board grid workers
  where grid = fromList [(Position (x, y), Space Ground Nothing) | x <- [XA .. XE], y <- [Y1 .. Y5]]
        workers = fromList [(BlueMan, NotOnBoard), (BlueWoman, NotOnBoard), (IvoryMan, NotOnBoard), (IvoryWoman, NotOnBoard)]

workersForPlayer :: Player -> [Worker]
workersForPlayer BluePlayer = [BlueMan, BlueWoman]
workersForPlayer IvoryPlayer = [IvoryMan, IvoryWoman]

nextPlayer :: Player -> Player
nextPlayer BluePlayer = IvoryPlayer
nextPlayer IvoryPlayer = BluePlayer

workersInPlacementOrder :: [Worker]
workersInPlacementOrder = [BlueMan, IvoryMan, BlueWoman, IvoryWoman]

spaceOnBoard :: Position -> Board -> Space
spaceOnBoard position board = (board.grid) ! position

buildUp :: Worker -> Position -> Board -> Either BoardError Board
buildUp buildWorker targetPosition board =
  spaceHasNoWorker targetSpace board
  >> spaceIsAdjacent buildWorker targetPosition board
  >> spaceCanBuildUp targetSpace board
  >> Right (board { grid = updatedGrid })
  where targetSpace = spaceOnBoard targetPosition board
        updatedGrid = insert targetPosition (targetSpace { level = succ targetSpace.level }) board.grid

nextWorkerToPlace :: Board -> Maybe Worker
nextWorkerToPlace board =
  if null listOfUnplacedWorkers
     then Nothing
     else Just $ head listOfUnplacedWorkers
  where workerIsPlaced worker = (board.workers) ! worker /= NotOnBoard
        listOfUnplacedWorkers = dropWhile workerIsPlaced workersInPlacementOrder

placeWorker :: Worker -> Position -> Board -> Either BoardError Board
placeWorker workerToPlace targetPosition board =
  workerCanBePlaced workerToPlace board
  >> spaceHasNoWorker targetSpace board
  >> Right (board { grid = updatedGrid, workers = updatedWorkers })
  where targetSpace = spaceOnBoard targetPosition board
        updatedGrid = insert targetPosition (targetSpace { worker = Just workerToPlace }) board.grid
        updatedWorkers = insert workerToPlace targetPosition board.workers

placeNextWorker :: Position -> Board -> Either BoardError Board
placeNextWorker targetPosition board =
  case nextWorkerToPlace board of
    Just worker -> placeWorker worker targetPosition board
    Nothing     -> Left $ AllWorkersPlacedError "No workers left to place"

moveWorker :: Worker -> Position -> Board -> Either BoardError Board
moveWorker workerToMove targetPosition board =
  spaceHasNoWorker targetSpace board
  >> spaceIsAdjacent workerToMove targetPosition board
  >> spaceCanBeMovedInto targetSpace board
  >> Right (board { grid = updatedGrid, workers = updatedWorkers })
  where targetSpace = spaceOnBoard targetPosition board
        originPosition = (board.workers) ! workerToMove
        originSpace = spaceOnBoard originPosition board
        updatedOriginSpace = (originPosition, originSpace { worker = Nothing })
        updatedTargetSpace = (targetPosition, targetSpace { worker = Just workerToMove })
        updatedGrid = insertMany [updatedOriginSpace, updatedTargetSpace] board.grid
        updatedWorkers = insert workerToMove targetPosition board.workers

spaceHasNoWorker :: Space -> Board -> Either BoardError Board
spaceHasNoWorker space board =
  case space.worker of
    Just _    -> Left $ OccupiedError "Worker exists in this space"
    Nothing   -> Right board

spaceCanBuildUp :: Space -> Board -> Either BoardError Board
spaceCanBuildUp space board =
  case space.level of
    Dome      -> Left $ BuildError "Can't build on top of a dome"
    _         -> Right board

spaceCanBeMovedInto :: Space -> Board -> Either BoardError Board
spaceCanBeMovedInto space board =
  spaceCanBuildUp space board
  >> spaceHasNoWorker space board

workerCanBePlaced :: Worker -> Board -> Either BoardError Board
workerCanBePlaced worker board =
  case (board.workers) ! worker of
    Position _ -> Left $ AlreadyPlacedWorkerError "Can't placed worker that's already on the board"
    NotOnBoard -> Right board

spaceIsAdjacent :: Worker -> Position -> Board -> Either BoardError Board
spaceIsAdjacent _ NotOnBoard _ = Left $ InvalidPositionError "Invalid position supplied to spaceIsAdjacent"
spaceIsAdjacent worker (Position (xTarget, yTarget)) board =
  let xTargetInt = fromEnum xTarget
      yTargetInt = fromEnum yTarget
      xTargetBounds = [xTargetInt - 1, xTargetInt, xTargetInt + 1]
      yTargetBounds = [yTargetInt - 1, yTargetInt, yTargetInt + 1]
   in case (board.workers) ! worker of
        Position (x, y) -> if fromEnum x `elem` xTargetBounds && fromEnum y `elem` yTargetBounds && (x, y) /= (xTarget, yTarget)
                              then Right board
                              else Left $ TargetSpaceNotAdjacentError "Target space needs to be adjacent to worker"
        NotOnBoard      -> Left $ WorkerNotYetPlacedError "Worker needs to be placed to check for adjacency"
