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
          [ (Position (XA, Y1), Space LevelOne (JustWorker IvoryMan))
          , (Position (XC, Y2), Space LevelThree (JustWorker BlueWoman))
          , (Position (XC, Y4), Space LevelTwo NoWorker)
          , (Position (XD, Y5), Space Dome NoWorker)
          ]
    let modifiedGrid = insertMany gridChanges emptyBoardFactory.grid
    let modifiedBoard = emptyBoardFactory { grid = modifiedGrid }

    it "returns the space at the specified position" $ do
      spaceOnBoard modifiedBoard (Position (XA, Y1)) `shouldBe` Space LevelOne (JustWorker IvoryMan)
      spaceOnBoard modifiedBoard (Position (XC, Y2)) `shouldBe` Space LevelThree (JustWorker BlueWoman)
      spaceOnBoard modifiedBoard (Position (XC, Y4)) `shouldBe` Space LevelTwo NoWorker
      spaceOnBoard modifiedBoard (Position (XD, Y5)) `shouldBe` Space Dome NoWorker
