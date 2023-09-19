module EngineSpec (spec) where

import Test.Hspec
import Engine
import BoardFactory

spec :: Spec
spec = do
  describe "emptyBoard" $ do
    it "returns a board with no buildings and no workers" $ do
      emptyBoard `shouldBe` emptyBoardFactory
