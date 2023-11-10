module DisplayEngine 
  (
  ) where

import Brick (simpleMain)

-- TODO: Re-add GameWidget when ready to implement multiple widgets
-- import Widgets.GameWidget (gameWidget)
import Widgets.BoardWidget (boardWidget)

main :: IO ()
main = simpleMain $ boardWidget
