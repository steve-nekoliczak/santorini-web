module DisplayEngine 
  (
  ) where

import Brick (simpleMain)

import Widgets.GameWidget (gameWidget)

main :: IO ()
main = simpleMain $ gameWidget
