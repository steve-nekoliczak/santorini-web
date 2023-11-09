module DisplayEngine 
  (
  ) where

import Brick (Widget, simpleMain, (<+>), (<=>), str, withBorderStyle, joinBorders, hLimitPercent)
import Brick.Widgets.Center (center)
import Brick.Widgets.Border (borderWithLabel, vBorder)
import Brick.Widgets.Border.Style (unicode)

boardWidget :: Widget ()
boardWidget = 
    joinBorders $
    withBorderStyle unicode $
    borderWithLabel (str "Board") $
    center (str "TODO: Board goes here")

hudWidget :: Widget ()
hudWidget =
    joinBorders $
    withBorderStyle unicode $
    borderWithLabel (str "HUD") $
    center (str "TODO: HUD goes here")

gameLogWidget :: Widget ()
gameLogWidget =
    joinBorders $
    withBorderStyle unicode $
    borderWithLabel (str "Text") $
    center (str "TODO: Game log goes here")

leftSide :: Widget ()
leftSide = boardWidget

rightSidePercent :: Int
rightSidePercent = 40

rightSide :: Widget ()
rightSide = hLimitPercent rightSidePercent $ hudWidget <=> gameLogWidget

main :: IO ()
main = simpleMain $ leftSide <+> rightSide
