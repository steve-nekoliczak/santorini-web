module EngineSpec (spec) where

import Test.Hspec
import Engine
import BoardFactory
import Lib

spec :: Spec
spec = do
  describe "emptyBoard" $ do
    it "returns a board with no buildings and no workers" $ do
      emptyBoard `shouldBe` emptyBoardFactory

  describe "spaceOnBoard" $ do
    let gridChanges =
          [ (Position (XA, Y1), Space LevelOne (Just $ Worker "p1a"))
          , (Position (XC, Y2), Space LevelThree (Just $ Worker "p2b"))
          , (Position (XC, Y4), Space LevelTwo Nothing)
          , (Position (XD, Y5), Space Dome Nothing)
          ]
    let modifiedGrid = insertMany gridChanges (grid emptyBoardFactory)
    let modifiedBoard = emptyBoardFactory { grid = modifiedGrid }

    it "returns the space at the specified position" $ do
      spaceOnBoard modifiedBoard (Position (XA, Y1))
        `shouldBe` Space LevelOne (Just $ Worker "p1a")
      spaceOnBoard modifiedBoard (Position (XC, Y2))
        `shouldBe` Space LevelThree (Just $ Worker "p2b")
      spaceOnBoard modifiedBoard (Position (XC, Y4))
        `shouldBe` Space LevelTwo Nothing
      spaceOnBoard modifiedBoard (Position (XD, Y5))
        `shouldBe` Space Dome Nothing
