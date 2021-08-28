module Test.Main where

import ZPrelude

import Effect (Effect, forE, foreachE)
import Effect.Class.Console (log)
import Data.Array (length, (!!), (..), zipWith)
import Data.Maybe
import Data.Foldable
import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert as Assert
import Test.Unit.Main (runTest)

import Tables

main :: Effect Unit
main = do
  sizeTest
  subsetTest

sizeTest :: Effect Unit
sizeTest = runTest do
  suite "Check for correct table sizes" do
    for_ tables \{name, tab} -> do
      let tablen = length tab
      test (name <> ", table size") do
        Assert.equal tabsize tablen
      for_ (0 .. (tablen - 1)) \i -> do
        test (name <> ", row = " <> show i) do
          Assert.equal (Just tabsize) (length <$> tab !! i)

-- Cayley table subset check
subsetTest :: Effect Unit
subsetTest = runTest do
  suite "Table subset check" do
    fromMaybe (pure unit)
      (subTest <$> tables !! 0 <*> tables !! 1)
    fromMaybe (pure unit)
      (subTest <$> tables !! 2 <*> tables !! 3)

subTest :: NamedTable -> NamedTable -> TestSuite
subTest t1 t2 = do
  test (t2.name <> " is a subset of " <> t1.name) (pure unit)
  for_ (zipAll t1.tab t2.tab) \s -> do
    test s (pure unit)

zipAll ::
  Array (Array String) -> Array (Array String) -> Array String
zipAll t1 t2 = zipWith zipEq t1 t2

zipEq :: Array String -> Array String -> String
zipEq t1 t2 = fold <| zipWith eqTest t1 t2

eqTest :: String -> String -> String
eqTest t1 t2
  | t1 == t2  = " ="
  | t2 == "0" = " 0"
  | otherwise = " X"
