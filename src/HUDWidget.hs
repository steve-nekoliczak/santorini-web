module HUDWidget
  ( hudWidget
  ) where

import Brick (Widget, joinBorders, str, withBorderStyle)
import Brick.Widgets.Border (borderWithLabel)
import Brick.Widgets.Border.Style (unicode)
import Brick.Widgets.Center (center)

hudWidget :: Widget ()
hudWidget =
    joinBorders $
    withBorderStyle unicode $
    borderWithLabel (str "HUD") $
    center (str "TODO: HUD goes here")
