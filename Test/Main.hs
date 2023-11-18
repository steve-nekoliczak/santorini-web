module Test.Main where

import Test.Hspec
import IHP.Prelude

import Test.EngineSpec

main :: IO ()
main = hspec do
    Test.EngineSpec.spec
