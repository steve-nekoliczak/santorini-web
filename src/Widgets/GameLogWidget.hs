module Widgets.GameLogWidget
  ( gameLogWidget
  ) where

import Brick (Widget, joinBorders, str, withBorderStyle)
import Brick.Widgets.Border (borderWithLabel)
import Brick.Widgets.Border.Style (unicode)
import Brick.Widgets.Center (center)

gameLogWidget :: Widget ()
gameLogWidget =
    joinBorders $
    withBorderStyle unicode $
    borderWithLabel (str "Text") $
    center (str "TODO: Game log goes here")
