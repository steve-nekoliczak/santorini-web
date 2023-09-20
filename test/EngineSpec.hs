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
          [ (Position (XCoord 'A', YCoord 1), Space LevelOne (Just $ Worker "p1a"))
          , (Position (XCoord 'C', YCoord 2), Space LevelThree (Just $ Worker "p2b"))
          , (Position (XCoord 'C', YCoord 4), Space LevelTwo Nothing)
          , (Position (XCoord 'D', YCoord 5), Space Dome Nothing)
          ]
    let modifiedGrid = insertMany gridChanges (grid emptyBoardFactory)
    let modifiedBoard = emptyBoardFactory { grid = modifiedGrid }

    it "returns the space at the specified position" $ do
      spaceOnBoard modifiedBoard (Position (XCoord 'A', YCoord 1))
        `shouldBe` Space LevelOne (Just $ Worker "p1a")
      spaceOnBoard modifiedBoard (Position (XCoord 'C', YCoord 2))
        `shouldBe` Space LevelThree (Just $ Worker "p2b")
      spaceOnBoard modifiedBoard (Position (XCoord 'C', YCoord 4))
        `shouldBe` Space LevelTwo Nothing
      spaceOnBoard modifiedBoard (Position (XCoord 'D', YCoord 5))
        `shouldBe` Space Dome Nothing
