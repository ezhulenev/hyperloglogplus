{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RoleAnnotations     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE TypeFamilies        #-}

module Data.HyperLogLogPlus.Type
  (
    HyperLogLogPlus()
  , insertHash
  , size
  ) where

import Numeric (showHex, showIntAtBase)
import Data.Char (intToDigit)
import Text.Printf

import           Control.Monad

import           Data.HyperLogLogPlus.Config

import           Data.Set                    (Set)
import qualified Data.Set                    as Set

import           Data.Vector                 ((!))
import qualified Data.Vector                 as DV
import qualified Data.Vector.Unboxed         as V
import qualified Data.Vector.Unboxed.Mutable as MV

import           Data.Proxy
import           Data.Semigroup
import           Data.Word
import           Data.Bits
import           Data.Bits.Extras
import           Data.Digest.Murmur64

import           GHC.TypeLits
import           GHC.Int

-- Example:
-- >>> :set -XDataKinds
-- >>> :load Data.HyperLogLogPlus
-- >>> mempty :: HyperLogLogPlus 5 5

-- | HyperLogLogPlus cardinality estimation paired with MinHash for intersection estimation
-- p - precision of HLL structure
-- k - precision of MinHash structure (size)
data HyperLogLogPlus (p :: Nat) (k :: Nat) = HyperLogLogPlus
  { hllRank   :: V.Vector Int8
  , hllMinSet :: Set Hash64
  } deriving (Eq)

type role HyperLogLogPlus nominal nominal

instance Semigroup (HyperLogLogPlus p k) where
  HyperLogLogPlus lrnk lset <> HyperLogLogPlus rrnk rset = undefined

instance (KnownNat p, KnownNat k, 4 <= p, p <= 18) => Monoid (HyperLogLogPlus p k) where
  mempty = HyperLogLogPlus (V.replicate (numBuckets p') 0) Set.empty
    where p' = natVal (Proxy :: Proxy p)
  mappend = (<>)

instance (KnownNat p, KnownNat k) => Show (HyperLogLogPlus p k) where
  show (HyperLogLogPlus rank minSet) = "HyperLogLogPlus [p = " ++ p' ++ " k = " ++ k' ++ " ] [ minSet size = " ++ s ++ " ]"
    where p' = show $ natVal (Proxy :: Proxy p)
          k' = show $ natVal (Proxy :: Proxy k)
          s = show $ Set.size minSet

insert :: forall p k a . (KnownNat p, KnownNat k, Hashable64 a) => a -> HyperLogLogPlus p k -> HyperLogLogPlus p k
insert e = insertHash (hash64 e)

insertHash :: forall p k . (KnownNat p, KnownNat k) => Hash64 -> HyperLogLogPlus p k -> HyperLogLogPlus p k
insertHash hash (HyperLogLogPlus rank minSet) = HyperLogLogPlus rank' minSet'
  where p' = fromIntegral $ natVal (Proxy :: Proxy p)
        k' = fromIntegral $ natVal (Proxy :: Proxy k)
        idx = bucketIdx p' hash
        rnk = calcRank p' hash
        rank' = V.modify (\mv -> do
                  old <- MV.read mv idx
                  when (rnk > old) $ MV.write mv idx rnk
                ) rank
        minSet' | Set.size s' > k' = Set.deleteMax s'
                | otherwise        = s'
                where s' = Set.insert hash minSet

size :: forall p k . (KnownNat p, KnownNat k) => HyperLogLogPlus p k -> Word64
size hll@(HyperLogLogPlus rank minSet)
  | ss < k'   = fromIntegral ss
  | otherwise = round $ estimatedSize hll
  where k' = fromIntegral $ natVal (Proxy :: Proxy k)
        ss = Set.size minSet

-- | Compute estimted size based on HLL
estimatedSize :: forall p k . (KnownNat p, KnownNat k) => HyperLogLogPlus p k -> Double
estimatedSize (HyperLogLogPlus rank minSet)
  | h <= thresholds ! idx = h
  | otherwise             = e
  where p' = natVal (Proxy :: Proxy p)
        idx = fromIntegral $ p' - minP
        nb = numBuckets p'
        q :: Double
        q =  fromIntegral nb
        nz :: Double
        nz = fromIntegral . V.length . V.filter (==0) $ rank
        s = V.sum . V.map (\r -> 2.0 ^^ (negate r)) $ rank
        ae = (alpha nb) * (q ^ 2) * (1.0 / s)
        e | ae < 5 * q = ae - estimatedBias ae (fromIntegral p')
          | otherwise  = ae
        h | nz > 0     = q * log (q / nz)
          | otherwise  =  e

-- | Returns an estimate of the bias given our current
-- estimate of size and our precision.
-- This performs a simple linear interpolation based
-- on empirical results.
estimatedBias :: Double -> Integer -> Double
estimatedBias e p
  | e <= DV.head red = DV.head bd
  | e >  DV.last red = 0.0
  | otherwise        = case idx of
                         Just j -> (slope * e) + intercept
                           where slope = (bd ! (j +1) - bd ! j) / (red ! (j + 1) - red ! j)
                                 intercept = bd ! (j + 1) - slope * red ! (j + 1)
                         Nothing -> 0.0
  where i = fromIntegral $ p - minP
        red = rawEstimateData ! i
        bd = biasData ! i
        idx = V.find (\x -> red ! x < e && e < red ! (x + 1)) $ V.enumFromN 0 (DV.length red - 2)

-- | Compute bucket index for given HLL precision level
bucketIdx :: Integer -> Hash64 -> Int
bucketIdx p h = fromIntegral $ shiftR (asWord64 h) (64 - fromIntegral p)

-- | Compute hash rank for given HLL precision level
calcRank :: Integer -> Hash64 -> Int8
calcRank p h = 1 + lz
  where lz = fromIntegral $ nlz $ shiftL (asWord64 h) $ fromIntegral p
