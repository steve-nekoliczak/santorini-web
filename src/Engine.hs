module Engine
  (
  ) where

import Prelude hiding (lookup)
import Data.Map hiding (map)

newtype BuildError = BuildError String deriving (Show)
newtype MoveError = MoveError String deriving (Show)

newtype XCoord = XCoord Char deriving (Show, Eq, Ord)
newtype YCoord = YCoord Int deriving (Show, Eq, Ord)
data Position = NotOnBoard | Position (XCoord, YCoord) deriving (Eq, Ord)

instance Show Position where
  show NotOnBoard = "Not on board"
  show (Position (XCoord x, YCoord y)) = x:(show y)

data Worker = Worker { name :: String } deriving (Show, Eq, Ord)

data Level = Ground | LevelOne | LevelTwo | LevelThree | Dome deriving (Show, Eq)

data Top = Top
class Stackable a where
  step :: a -> Either Top a
  curr :: a -> Either Top a

instance Stackable Level where
  step Ground = Right LevelOne
  step LevelOne = Right LevelTwo
  step LevelTwo = Right LevelThree
  step LevelThree = Right Dome
  step Dome = Left Top

  curr Dome = Left Top
  curr level = Right level

data Space = Empty { level :: Level } | Occupied { level :: Level, worker ::  Worker } deriving (Show, Eq)


data Board = Board { grid :: (Map Position Space)
                   , workers :: (Map Worker Position)
                   }  deriving (Show, Eq)

xCoords :: [XCoord]
xCoords = [XCoord 'A', XCoord 'B', XCoord 'C', XCoord 'D', XCoord 'E']

yCoords :: [YCoord]
yCoords = [YCoord 1, YCoord 2, YCoord 3, YCoord 4, YCoord 5]

emptyBoard :: Board 
emptyBoard = Board grid workers
  where grid = fromList [(Position (x, y), Empty Ground) | x <- xCoords, y <- yCoords]
        workers = fromList [(Worker "p1a", NotOnBoard), (Worker "p1b", NotOnBoard), (Worker "p2a", NotOnBoard), (Worker "p2b", NotOnBoard)]


spaceOnBoard :: Board -> Position -> Space
spaceOnBoard (Board { grid = grid }) position = grid ! position

buildUp :: Board -> Position -> Either BuildError Board 
buildUp board position =
  case spaceOnBoard board position of
    Occupied _ _  -> Left $ BuildError "Can't build where a worker is"
    Empty level   ->
      case step level of
        Left Top        -> Left $ BuildError "Can't build on top of a dome"
        Right newLevel  -> Right $ board { grid = newGrid }
          where newGrid = insert position (Empty newLevel) (grid board)

insertMany :: Ord k => [(k, a)] -> (Map k a) -> (Map k a)
insertMany kvs m = Prelude.foldr (\(k, v) m -> insert k v m) m kvs


moveWorker :: Board -> Worker -> Position -> Either MoveError Board
moveWorker board worker newPosition =
  case newSpace of
    Occupied _ _        -> Left $ MoveError "Can't move where a worker is"
    Empty newLevel      ->
      case curr newLevel of
        Left Top        -> Left $ MoveError "Can't move on top of a dome"
        Right newLevel  -> Right $ board { grid = newGrid, workers = insert worker newPosition (workers board) }
          where newGrid = insertMany [oldSpaceEntry, newSpaceEntry] (grid board)
                oldSpaceEntry = (oldPosition, Empty $ level oldSpace)
                newSpaceEntry = (newPosition, Occupied newLevel worker)
    where oldSpace = spaceOnBoard board oldPosition
          newSpace = spaceOnBoard board newPosition
          oldPosition = (workers board) ! worker

placeWorker :: Board -> Worker -> Position -> Either MoveError Board
placeWorker board worker newPosition =
  case ((workers board) ! worker) of
    Position _  -> Left $ MoveError "Can't place worker that's already on the board"
    NotOnBoard  ->
      case newSpace of
        Occupied _ _    -> Left $ MoveError "Can't place a worker where a worker is"
        Empty newLevel  -> Right $ board { grid = insert newPosition (Occupied newLevel worker) (grid board), workers = insert worker newPosition (workers board) }
      where newSpace = spaceOnBoard board newPosition
