{-# LANGUAGE DataKinds #-}

module Data.HyperLogLogPlus.Tests
  (
    tests
  ) where

import           Data.HyperLogLogPlus
import           Test.Tasty              (TestTree, testGroup)
import           Test.Tasty.HUnit        (testCase)
import           Test.HUnit              (Assertion, assertEqual)

tests :: TestTree
tests = testGroup "Data.HyperLogLogPlus"
    [ testCase "testMempty" testMempty ]

testMempty :: Assertion
testMempty = assertEqual "" (size (mempty :: HyperLogLogPlus 12 8192)) 0
