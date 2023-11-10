module BoardWidget
  ( boardWidget
  ) where

import Brick (Widget, Size, str, txt, withBorderStyle, joinBorders, hLimit, vLimit)
import Brick.Widgets.Center (center)
import Brick.Widgets.Border (borderWithLabel)
import Brick.Widgets.Border.Style (unicode)
import Brick.Widgets.Table (table, renderTable)
import qualified Data.Text as T
import Engine
import Data.List.Split (chunksOf)

boardWidget :: Widget ()
boardWidget =
    joinBorders $
    withBorderStyle unicode $
    borderWithLabel (str "Board") $
    center (renderBoard emptyBoard)

renderBoard :: Board -> Widget ()
renderBoard board = renderTable $ table $
  chunksOf 5 $ map (renderSpace board) [Position (x, y) | y <- reverse [Y1 .. Y5], x <- [XA .. XE]]

renderSpace :: Board -> Position -> Widget ()
renderSpace board position  = hLimit 20 $ vLimit 10 $ center (txt $ T.pack $ show position)
