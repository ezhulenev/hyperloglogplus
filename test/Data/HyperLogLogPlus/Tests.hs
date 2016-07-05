{-# LANGUAGE DataKinds #-}

module Data.HyperLogLogPlus.Tests
  (
    tests
  ) where

import           Data.HyperLogLogPlus
import           Data.Semigroup
import           Data.Maybe

import           Test.Tasty              (TestTree, testGroup)
import           Test.Tasty.HUnit        (testCase)
import           Test.HUnit              (Assertion, assertEqual, assertBool)

tests :: TestTree
tests = testGroup "Data.HyperLogLogPlus"
    [ testCase "testMempty" testMempty
    , testCase "testSmallSet" testSmallSet
    , testCase "testBigSet" testBigSet
    , testCase "testSemigroup" testSemigroup
    , testCase "testIntersection" testIntersection
    , testCase "testUpcastP" testUpcastP
    , testCase "testUpcastK" testUpcastK
    , testCase "testUpcastK'" testUpcastK'
    , testCase "testDowncast" testDowncast
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

testIntersection :: Assertion
testIntersection = assertBool "" $ 4900 < n && n < 5100
  where hll1 = foldr insert mempty (gen [1 .. 35000]) :: HLL
        hll2 = foldr insert mempty (gen [30000 .. 50000]) :: HLL
        n = intersection [hll1, hll2]

testUpcastP :: Assertion
testUpcastP = assertBool "" $ isNothing casted
  where hll = foldr insert mempty (gen [1 .. 5000]) :: HyperLogLogPlus 12 1024
        casted = cast hll :: Maybe(HyperLogLogPlus 14 1024)

-- reached min-set limit
testUpcastK :: Assertion
testUpcastK = assertBool "" $ isNothing casted
  where hll = foldr insert mempty (gen [1 .. 5000]) :: HyperLogLogPlus 12 1024
        casted = cast hll :: Maybe(HyperLogLogPlus 12 2048)

-- didn't reach original min-set limit
testUpcastK' :: Assertion
testUpcastK' = assertBool "" $ isJust casted
  where hll = foldr insert mempty (gen [1 .. 500]) :: HyperLogLogPlus 12 1024
        casted = cast hll :: Maybe(HyperLogLogPlus 12 2048)

testDowncast :: Assertion
testDowncast = assertBool "" $ 14000 < n && n < 16000
  where hll = foldr insert mempty (gen [1 .. 15000]) :: HyperLogLogPlus 12 4096
        casted = cast hll :: Maybe(HyperLogLogPlus 10 2048)
        n = fromJust $ fmap size casted

gen :: (Show a) => [a] -> [String]
gen = map show
