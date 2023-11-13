{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

module Widgets.ControlDialogWidget where

import Lens.Micro ((^.))
import Lens.Micro.Mtl
import Lens.Micro.TH

import qualified Graphics.Vty as V

import qualified Brick.Main as M
import Brick.Types
  ( Widget
  , BrickEvent(..)
  )
import Brick.Widgets.Core
  ( padAll
  , str
  )
import qualified Brick.Widgets.Dialog as D
import qualified Brick.Widgets.Center as C
import qualified Brick.AttrMap as A
import Brick.Util (on, bg)
import qualified Brick.Types as T

data XPlacementChoice = ChoiceXA | ChoiceXB | ChoiceXC | ChoiceXD | ChoiceXE
            deriving (Show, Eq, Ord)

data XPlacementName =
    XAButton
  | XBButton
  | XCButton
  | XDButton
  | XEButton
  deriving (Show, Eq, Ord)

data YPlacementChoice = ChoiceY1 | ChoiceY2 | ChoiceY3 | ChoiceY4 | ChoiceY5
            deriving Show

data YPlacementName =
    Y1Button
  | Y2Button
  | Y3Button
  | Y4Button
  | Y5Button
  deriving (Show, Eq, Ord)

data DisplayState =
  PickXCoord { _xCoordDialog :: D.Dialog XPlacementChoice () }
  | PickYCoord

-- makeLenses ''DialogState

-- All widgets returned by drawUI need to be of the same type.
-- AllWidgets is a list of the names of the widgets.
drawUI :: DisplayState -> [Widget ()]
drawUI (PickXCoord xCoordDialog) = [ui]
    where
        ui = D.renderDialog (xCoordDialog) $ C.hCenter $ padAll 1 $ str "This is the dialog body."

-- appEvent :: BrickEvent () e -> T.EventM () DialogState ()
-- appEvent (VtyEvent ev) =
--     case ev of
--         V.EvKey V.KEsc [] -> M.halt
--         V.EvKey V.KEnter [] -> T.put initialState
--         -- _ -> D.handleDialogEvent ev
-- appEvent _ = return ()


xState :: Maybe (XPlacementName, [(String, XPlacementName, XPlacementChoice)])
xState = Just (XAButton, xPlacementChoices)

xPlacementChoices :: [(String, XPlacementName, XPlacementChoice)]
xPlacementChoices =
  [ ("A",   XAButton, ChoiceXA)
  , ("B",   XBButton, ChoiceXB)
  , ("C",   XCButton, ChoiceXC)
  , ("D",   XDButton, ChoiceXD)
  , ("E",   XEButton, ChoiceXE)
  ]

-- yState :: Ord name => Maybe (name, [(String, name, choice)])
-- yState = Just (Y1Button, yPlacementChoices)

-- yPlacementChoices :: [(String, name, choice)]
-- yPlacementChoices =
--   [ ("1",   Y1Button, ChoiceY1)
--   , ("2",   Y2Button, ChoiceY2)
--   , ("3",   Y3Button, ChoiceY3)
--   , ("4",   Y4Button, ChoiceY4)
--   , ("5",   Y5Button, ChoiceY5)
--   ]

initialState :: DisplayState
initialState =
  PickXCoord $ D.dialog (Just $ str "Title") (Just ((), [("A", (), ChoiceXA)])) 50

theMap :: A.AttrMap
theMap = A.attrMap V.defAttr
    [ (D.dialogAttr, V.white `on` V.blue)
    , (D.buttonAttr, V.black `on` V.white)
    , (D.buttonSelectedAttr, bg V.yellow)
    ]

appEvent :: BrickEvent () e -> T.EventM () (DisplayState) ()
appEvent (VtyEvent ev) =
    case ev of
        V.EvKey V.KEsc [] -> M.halt
        V.EvKey V.KEnter [] -> M.halt
        -- _ -> D.handleDialogEvent ev
appEvent _ = return ()

-- App state event name
theApp :: M.App DisplayState e ()
theApp =
    M.App { M.appDraw = drawUI
          , M.appChooseCursor = M.showFirstCursor
          , M.appHandleEvent = appEvent
          , M.appStartEvent = return ()
          , M.appAttrMap = const theMap
          }

-- main :: (Ord name, Show name, Show choice) => IO (D.Dialog choice name)
main :: IO ()
main = do
    -- d <- M.defaultMain theApp (nextState (Just (XAButton, xPlacementChoices)))
    d <- M.defaultMain theApp initialState
    -- putStrLn $ "You chose: " <> show (D.dialogSelection d)
    putStrLn $ "sup"
    -- return d
