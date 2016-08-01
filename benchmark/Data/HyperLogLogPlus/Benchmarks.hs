{-# LANGUAGE DataKinds #-}

module Data.HyperLogLogPlus.Benchmarks
  (
    benchSemigroup
  , benchSize
  , benchIntersect
  , HLL
  ) where

import Data.HyperLogLogPlus
import Data.Word
import Data.Semigroup

import Criterion            (Benchmark, bench, nf, env)


type HLL = HyperLogLogPlus 12 2048

benchSemigroup :: Int -> Benchmark
benchSemigroup n =  bench (show n) $ nf f n
  where f n = size (foldr insert mempty (map show [1 .. n]) :: HLL)

benchSize :: Int -> HLL -> Benchmark
benchSize n hll = bench (show n) $ nf size hll
  where f :: HLL -> Word64
        f = size

benchIntersect :: Int -> HLL -> HLL -> Benchmark
benchIntersect n hll1 hll2 = bench (show n) $ nf f (hll1, hll2)
  where f :: (HLL, HLL) -> Word64
        f (l, r) = intersection [l, r]
