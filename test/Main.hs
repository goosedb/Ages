{-# LANGUAGE BlockArguments #-}

module Main (main) where

import Data.Time.Ages (Unit (..))
import Test.Hspec
import Data.List (sort)

main :: IO ()
main = hspec do
  describe "Units" do
    it "Should obey Enum instance expectation" do
      [minBound .. maxBound]
        `shouldBe` [ Picoseconds
                   , Nanoseconds
                   , Microseconds
                   , Milliseconds
                   , Seconds
                   , Minutes
                   , Hours
                   , Days
                   , Weeks
                   ]

    it "Should obey Ord instance expectation" do
      [minBound .. maxBound] `shouldBe` sort [minBound :: Unit .. maxBound]
