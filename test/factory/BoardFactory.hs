module BoardFactory (emptyBoardFactory) where

import Data.Map
import Engine

emptyBoardFactory :: Board
emptyBoardFactory = Board emptyGrid unplacedWorkers

emptyGrid :: Map Position Space
emptyGrid = fromList [ (Position (XCoord 'A', YCoord 1), Empty Ground)
                     , (Position (XCoord 'A', YCoord 2), Empty Ground)
                     , (Position (XCoord 'A', YCoord 3), Empty Ground)
                     , (Position (XCoord 'A', YCoord 4), Empty Ground)
                     , (Position (XCoord 'A', YCoord 5), Empty Ground)
                     , (Position (XCoord 'B', YCoord 1), Empty Ground)
                     , (Position (XCoord 'B', YCoord 2), Empty Ground)
                     , (Position (XCoord 'B', YCoord 3), Empty Ground)
                     , (Position (XCoord 'B', YCoord 4), Empty Ground)
                     , (Position (XCoord 'B', YCoord 5), Empty Ground)
                     , (Position (XCoord 'C', YCoord 1), Empty Ground)
                     , (Position (XCoord 'C', YCoord 2), Empty Ground)
                     , (Position (XCoord 'C', YCoord 3), Empty Ground)
                     , (Position (XCoord 'C', YCoord 4), Empty Ground)
                     , (Position (XCoord 'C', YCoord 5), Empty Ground)
                     , (Position (XCoord 'D', YCoord 1), Empty Ground)
                     , (Position (XCoord 'D', YCoord 2), Empty Ground)
                     , (Position (XCoord 'D', YCoord 3), Empty Ground)
                     , (Position (XCoord 'D', YCoord 4), Empty Ground)
                     , (Position (XCoord 'D', YCoord 5), Empty Ground)
                     , (Position (XCoord 'E', YCoord 1), Empty Ground)
                     , (Position (XCoord 'E', YCoord 2), Empty Ground)
                     , (Position (XCoord 'E', YCoord 3), Empty Ground)
                     , (Position (XCoord 'E', YCoord 4), Empty Ground)
                     , (Position (XCoord 'E', YCoord 5), Empty Ground)
                     ]

unplacedWorkers :: Map Worker Position
unplacedWorkers = fromList [ (Worker "p1a", NotOnBoard)
                           , (Worker "p1b", NotOnBoard)
                           , (Worker "p2a", NotOnBoard)
                           , (Worker "p2b", NotOnBoard)
                           ]
