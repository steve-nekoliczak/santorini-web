{-# LANGUAGE OverloadedRecordDot #-}

module DisplayEngine 
  (
  ) where

import Brick (Widget, Size, simpleMain, (<+>), (<=>), str, txt, withBorderStyle, joinBorders, hLimitPercent, hLimit, vLimit)
import Brick.Widgets.Center (center)
import Brick.Widgets.Border (borderWithLabel, vBorder)
import Brick.Widgets.Border.Style (unicode)
import Brick.Widgets.Table (table, renderTable)
import Engine
import qualified Data.Text as T
import Data.Map hiding (map, null)
import Data.List.Split (chunksOf)

boardWidget :: Widget ()
boardWidget = 
    joinBorders $
    withBorderStyle unicode $
    borderWithLabel (str "Board") $
    center (renderBoard emptyBoard)

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

renderBoard :: Board -> Widget ()
renderBoard board = renderTable $ table $
  chunksOf 5 $ map (renderSpace board) [Position (x, y) | y <- reverse [Y1 .. Y5], x <- [XA .. XE]]

renderSpace :: Board -> Position -> Widget ()
renderSpace board position  = hLimit 20 $ vLimit 10 $ center (txt $ T.pack $ show position)

main :: IO ()
main = simpleMain $ leftSide <+> rightSide
