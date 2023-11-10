module Widgets.MainWidget
  ( mainWidget
  ) where

import Brick (Widget, (<+>), (<=>), hLimitPercent)

import Widgets.BoardWidget (boardWidget)
import Widgets.GameLogWidget (gameLogWidget)
import Widgets.HUDWidget (hudWidget)

leftSide :: Widget ()
leftSide = boardWidget

rightSidePercent :: Int
rightSidePercent = 40

rightSide :: Widget ()
rightSide = hLimitPercent rightSidePercent $ hudWidget <=> gameLogWidget

mainWidget :: Widget ()
mainWidget = leftSide <+> rightSide
