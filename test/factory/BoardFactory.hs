module BoardFactory (emptyBoardFactory) where

import Data.Map
import Engine

emptyBoardFactory :: Board
emptyBoardFactory = Board emptyGrid unplacedWorkers

emptyGrid :: Map Position Space
emptyGrid = fromList [ (Position (XA, Y1), Space Ground NoWorker)
                     , (Position (XA, Y2), Space Ground NoWorker)
                     , (Position (XA, Y3), Space Ground NoWorker)
                     , (Position (XA, Y4), Space Ground NoWorker)
                     , (Position (XA, Y5), Space Ground NoWorker)
                     , (Position (XB, Y1), Space Ground NoWorker)
                     , (Position (XB, Y2), Space Ground NoWorker)
                     , (Position (XB, Y3), Space Ground NoWorker)
                     , (Position (XB, Y4), Space Ground NoWorker)
                     , (Position (XB, Y5), Space Ground NoWorker)
                     , (Position (XC, Y1), Space Ground NoWorker)
                     , (Position (XC, Y2), Space Ground NoWorker)
                     , (Position (XC, Y3), Space Ground NoWorker)
                     , (Position (XC, Y4), Space Ground NoWorker)
                     , (Position (XC, Y5), Space Ground NoWorker)
                     , (Position (XD, Y1), Space Ground NoWorker)
                     , (Position (XD, Y2), Space Ground NoWorker)
                     , (Position (XD, Y3), Space Ground NoWorker)
                     , (Position (XD, Y4), Space Ground NoWorker)
                     , (Position (XD, Y5), Space Ground NoWorker)
                     , (Position (XE, Y1), Space Ground NoWorker)
                     , (Position (XE, Y2), Space Ground NoWorker)
                     , (Position (XE, Y3), Space Ground NoWorker)
                     , (Position (XE, Y4), Space Ground NoWorker)
                     , (Position (XE, Y5), Space Ground NoWorker)
                     ]

unplacedWorkers :: Map Worker Position
unplacedWorkers = fromList [ (BlueMan, NotOnBoard)
                           , (BlueWoman, NotOnBoard)
                           , (IvoryMan, NotOnBoard)
                           , (IvoryWoman, NotOnBoard)
                           ]
