module EngineSpec (spec) where

import Test.Hspec
import Engine
import BoardFactory
import Data.Either
import Data.Map hiding (map)

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
    let modifiedBoard = modifyEmptyBoard gridChanges

    it "returns the space at the specified position" $ do
      spaceOnBoard modifiedBoard (Position (XA, Y1)) `shouldBe` Space LevelOne (JustWorker IvoryMan)
      spaceOnBoard modifiedBoard (Position (XC, Y2)) `shouldBe` Space LevelThree (JustWorker BlueWoman)
      spaceOnBoard modifiedBoard (Position (XC, Y4)) `shouldBe` Space LevelTwo NoWorker
      spaceOnBoard modifiedBoard (Position (XD, Y5)) `shouldBe` Space Dome NoWorker

  describe "buildUp" $ do
    it "builds on a non-dome space" $ do
      let modifiedBoard = buildUp emptyBoardFactory (Position (XA, Y1))

      isRight modifiedBoard `shouldBe` True
      spaceOnBoard (fromRight emptyBoardFactory modifiedBoard) (Position (XA, Y1)) `shouldBe` Space LevelOne NoWorker

    it "returns an error when building on a dome" $ do
      let position = Position (XD, Y5)
      let modifiedBoard = modifyEmptyBoard [(position, Space Dome NoWorker)]
      let errorBoard = buildUp modifiedBoard position

      isLeft errorBoard `shouldBe` True
      errorBoard `shouldBe` (Left $ BuildError "Can't build on top of a dome")

  describe "placeWorker" $ do
    it "puts a worker on the board and updates the board's grid map and workers map" $ do
      let position = (Position (XC, Y4))
      let modifiedBoard = placeWorker emptyBoardFactory BlueMan position

      spaceOnBoard (fromRight emptyBoardFactory modifiedBoard) position `shouldBe` Space Ground (JustWorker BlueMan)
      (fromRight emptyBoardFactory modifiedBoard).workers ! BlueMan `shouldBe` position
