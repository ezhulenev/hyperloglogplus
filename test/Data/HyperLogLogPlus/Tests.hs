{-# LANGUAGE DataKinds #-}

module Data.HyperLogLogPlus.Tests
  (
    tests
  ) where

import           Data.HyperLogLogPlus
import           Data.Semigroup

import           Test.Tasty              (TestTree, testGroup)
import           Test.Tasty.HUnit        (testCase)
import           Test.HUnit              (Assertion, assertEqual, assertBool)

tests :: TestTree
tests = testGroup "Data.HyperLogLogPlus"
    [ testCase "testMempty" testMempty
    , testCase "testSmallSet" testSmallSet
    , testCase "testBigSet" testBigSet
    , testCase "testSemigroup" testSemigroup
    ]

type HLL = HyperLogLogPlus 12 8192

testMempty :: Assertion
testMempty = assertEqual "" (size (mempty :: HyperLogLogPlus 12 8192)) 0

testSmallSet :: Assertion
testSmallSet = assertBool "" $ size (foldr insert mempty (gen [1 .. 1234]) :: HLL) == 1234

testBigSet :: Assertion
testBigSet = assertBool "" $ 49000 < n && n < 51000
  where n = size (foldr insert mempty (gen [1 .. 50000]) :: HLL)

testSemigroup :: Assertion
testSemigroup = assertBool "" $ 49000 < n && n < 51000
  where hll1 = foldr insert mempty (gen [1 .. 35000]) :: HLL
        hll2 = foldr insert mempty (gen [15000 .. 50000]) :: HLL
        n = size $ hll1 <> hll2

gen :: (Show a) => [a] -> [String]
gen = map show
