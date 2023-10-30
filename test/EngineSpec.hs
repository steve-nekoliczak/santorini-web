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
      spaceOnBoard (Position (XA, Y1)) modifiedBoard `shouldBe` Space LevelOne (JustWorker IvoryMan)
      spaceOnBoard (Position (XC, Y2)) modifiedBoard `shouldBe` Space LevelThree (JustWorker BlueWoman)
      spaceOnBoard (Position (XC, Y4)) modifiedBoard `shouldBe` Space LevelTwo NoWorker
      spaceOnBoard (Position (XD, Y5)) modifiedBoard `shouldBe` Space Dome NoWorker

  describe "buildUp" $ do
    it "builds on a non-dome space" $ do
      let modifiedBoard = buildUp (Position (XA, Y1)) emptyBoardFactory

      isRight modifiedBoard `shouldBe` True
      spaceOnBoard (Position (XA, Y1)) (fromRight emptyBoardFactory modifiedBoard) `shouldBe` Space LevelOne NoWorker

    it "returns an error when building on a dome" $ do
      let position = Position (XD, Y5)
      let modifiedBoard = modifyEmptyBoard [(position, Space Dome NoWorker)]
      let errorBoard = buildUp position modifiedBoard

      isLeft errorBoard `shouldBe` True
      errorBoard `shouldBe` (Left $ BuildError "Can't build on top of a dome")

  describe "placeWorker" $ do
    it "puts a worker on the board and updates the board's grid map and workers map" $ do
      let position = (Position (XC, Y4))
      let modifiedBoard = placeWorker BlueMan position emptyBoardFactory

      spaceOnBoard position (fromRight emptyBoardFactory modifiedBoard) `shouldBe` Space Ground (JustWorker BlueMan)
      (fromRight emptyBoardFactory modifiedBoard).workers ! BlueMan `shouldBe` position

  describe "moveWorker" $ do
    it "moves a worker to another space of the same level" $ do
      let originPosition = (Position (XC, Y4))
      let targetPosition = (Position (XC, Y5))
      let modifiedBoard = placeWorker BlueMan originPosition emptyBoardFactory
      let boardAfterMove = moveWorker BlueMan targetPosition (fromRight emptyBoardFactory modifiedBoard)

      spaceOnBoard originPosition (fromRight emptyBoardFactory boardAfterMove) `shouldBe` Space Ground NoWorker
      spaceOnBoard targetPosition (fromRight emptyBoardFactory boardAfterMove) `shouldBe` Space Ground (JustWorker BlueMan)
      (fromRight emptyBoardFactory boardAfterMove).workers ! BlueMan `shouldBe` targetPosition
