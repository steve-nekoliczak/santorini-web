{-# LANGUAGE OverloadedRecordDot #-}

module DisplayEngine 
  (
  ) where

import Brick (Widget, simpleMain, (<+>), (<=>), hLimitPercent)
import BoardWidget (boardWidget)
import GameLogWidget (gameLogWidget)
import HUDWidget (hudWidget)

leftSide :: Widget ()
leftSide = boardWidget

rightSidePercent :: Int
rightSidePercent = 40

rightSide :: Widget ()
rightSide = hLimitPercent rightSidePercent $ hudWidget <=> gameLogWidget

main :: IO ()
main = simpleMain $ leftSide <+> rightSide
