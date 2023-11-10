module DisplayEngine 
  (
  ) where

import Brick (simpleMain)

import Widgets.MainWidget (mainWidget)

main :: IO ()
main = simpleMain $ mainWidget
