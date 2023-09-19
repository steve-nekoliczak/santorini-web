module EngineSpec (spec) where

import Test.Hspec
import Engine
-- TODO: Figure out how to fix this or what to use instead of factories.
import BoardFactory

spec :: Spec
spec = do
  describe "emptyBoard" $ do
    it "returns a board with no buildings and no workers" $ do
      emptyBoard `shouldBe` emptyBoardFactory
