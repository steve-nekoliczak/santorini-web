module DisplayEngine 
  (
  ) where

import Brick (Widget, (<+>), (<=>), hLimitPercent, simpleMain)

import Widgets.BoardWidget (boardWidget)
import Widgets.GameLogWidget (gameLogWidget)
import Widgets.HUDWidget (hudWidget)

leftSide :: Widget ()
leftSide = boardWidget

rightSidePercent :: Int
rightSidePercent = 40

rightSide :: Widget ()
rightSide = hLimitPercent rightSidePercent $ hudWidget <=> gameLogWidget

main :: IO ()
main = simpleMain $ leftSide <+> rightSide
