module Main where

import           Test.Tasty (defaultMain, testGroup)

import qualified Data.HyperLogLogPlus.Tests

main :: IO ()
main = defaultMain $ testGroup "HyperLogLogPlus Tests"
    [ Data.HyperLogLogPlus.Tests.tests ]
