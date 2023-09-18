import Prelude hiding (lookup)
import Data.Map hiding (map)



-- TODO:
-- Add to github repo santorini CLI
-- Add NotInPlay to Position ADT
-- Add placeWorker function


newtype BuildError = BuildError String deriving (Show)
newtype MoveError = MoveError String deriving (Show)

newtype XCoord = XCoord Char deriving (Show, Eq, Ord)
newtype YCoord = YCoord Int deriving (Show, Eq, Ord)
data Position = Position (XCoord, YCoord) deriving (Eq, Ord)

instance Show Position where
  show (Position (XCoord x, YCoord y)) = x:(show y)

newtype Player = Player Int deriving (Show, Eq)
data Worker = Worker Player Int deriving (Show, Eq)

data Level = Ground | LevelOne | LevelTwo | LevelThree | Dome deriving (Show, Eq)

data Top = Top
class Stackable a where
  step :: a -> Either Top a

instance Stackable Level where
  step Ground = Right LevelOne
  step LevelOne = Right LevelTwo
  step LevelTwo = Right LevelThree
  step LevelThree = Right Dome
  step Dome = Left Top

data Space = Empty { level :: Level } | Occupied { level :: Level, worker ::  Worker } deriving (Eq)

showGround = "|--------|\n|        |\n|        |\n|--------|"



instance Show Space where
  show (Empty Ground) = showGround 
  show (Empty LevelOne) = "----+"
  show (_) = "+++++"

data Board = Board { grid :: (Map Position Space)
                   }  deriving (Show, Eq)

xCoords :: [XCoord]
xCoords = [XCoord 'A', XCoord 'B', XCoord 'C', XCoord 'D', XCoord 'E']

yCoords :: [YCoord]
yCoords = [YCoord 1, YCoord 2, YCoord 3, YCoord 4, YCoord 5]

emptyBoard :: Board 
emptyBoard = Board grid
  where grid = fromList [(Position (x, y), Empty Ground) | x <- xCoords, y <- yCoords]


spaceOnBoard :: Board -> Position -> Space
spaceOnBoard (Board { grid = grid }) position = grid ! position

buildUp :: Board -> Position -> Either BuildError Board 
buildUp (Board grid) position =
  case spaceOnBoard (Board grid) position of
    Occupied _ _  -> Left $ BuildError "Can't build where a worker is"
    Empty level   ->
      case step level of
        Left Top        -> Left $ BuildError "Can't build on top of a dome"
        Right newLevel  -> Right $ Board newGrid
          where newGrid = (insert position (Empty newLevel) grid)



moveWorker :: Board -> Worker -> Position -> Position -> Either MoveError Board
moveWorker (Board grid) worker oldPosition newPosition =
  case newSpace of
    Occupied _ _  -> Left $ MoveError "Can't move where a worker is"
    Empty newLevel   ->
      case newLevel of
        Dome        -> Left $ MoveError "Can't move on top of a dome"
        newLevel       -> Right $ Board newGrid
          where newGrid = Prelude.foldr (\(pos, sp) acc -> insert pos sp acc) grid [updateNewSpace, updateOldSpace]
                updateNewSpace = (newPosition, (Occupied newLevel worker))
                updateOldSpace = (oldPosition, (Empty $ level oldSpace))
    where oldSpace = spaceOnBoard (Board grid) oldPosition
          newSpace = spaceOnBoard (Board grid) newPosition

-- placeWorker :: Board -> Worker -> Position -> Either MoveError Board
-- placeWorker (Board grid) worker position =
--   case targetSpace of
--     Occupied _ _  -> Left $ MoveError "Can't move where a worker is"
--     Empty newLevel   ->
--       case newLevel of
--         Dome        -> Left $ MoveError "Can't move on top of a dome"
--         newLevel       -> Right $ Board newGrid
--           where newGrid = Prelude.foldr (\(pos, sp) acc -> insert pos sp acc) grid [updateNewSpace, updateOldSpace]
--                 updateNewSpace = (newPosition, (Occupied newLevel worker))
--                 updateOldSpace = (oldPosition, (Empty $ level oldSpace))
--     where oldSpace = spaceOnBoard (Board grid) oldPosition
--           newSpace = spaceOnBoard (Board grid) position
-- 
