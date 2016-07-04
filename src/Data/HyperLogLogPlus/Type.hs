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
  , insert
  , insertHash
  , size
  , intersection
  ) where

import           Control.Monad

import           Data.HyperLogLogPlus.Config

import           Data.Set                    (Set)
import qualified Data.Set                    as Set
import           Data.List                   (find, maximum, foldl')

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
-- >>>
-- >>> type HLL = HyperLogLogPlus 12 8192
-- >>>
-- >>> mempty :: HLL
-- >>> size (foldr insert mempty [1 .. 75000] :: HLL)
-- >>> size $ (foldr insert mempty [1 .. 5000] ::  HLL) <> (foldr insert mempty [3000 .. 10000] :: HLL)

-- | HyperLogLogPlus cardinality estimation paired with MinHash for intersection estimation
-- p - precision of HLL structure
-- k - precision of MinHash structure (max size)
data HyperLogLogPlus (p :: Nat) (k :: Nat) = HyperLogLogPlus
  { hllRank   :: V.Vector Int8
  , hllMinSet :: Set Hash64
  } deriving (Eq)

type role HyperLogLogPlus nominal nominal

instance (KnownNat k) => Semigroup (HyperLogLogPlus p k) where
  a@(HyperLogLogPlus ar ah) <> b@(HyperLogLogPlus br bh) = HyperLogLogPlus (V.zipWith max ar br) (iterate Set.deleteMax u !! n)
    where k = fromIntegral $ kctx a
          u = Set.union ah bh
          n = max 0 (Set.size u - k)

instance (KnownNat p, KnownNat k, 4 <= p, p <= 18) => Monoid (HyperLogLogPlus p k) where
  mempty = HyperLogLogPlus (V.replicate (numBuckets p) 0) Set.empty
    where p = natVal (Proxy :: Proxy p)
  mappend = (<>)

instance (KnownNat p, KnownNat k) => Show (HyperLogLogPlus p k) where
  show hll@(HyperLogLogPlus rank minSet) = "HyperLogLogPlus [p = " ++ p ++ " k = " ++ k ++ " ] [ minSet size = " ++ s ++ " ]"
    where p = show $ pctx hll
          k = show $ kctx hll
          s = show $ Set.size minSet

insert :: forall p k a . (KnownNat p, KnownNat k, Hashable64 a) => a -> HyperLogLogPlus p k -> HyperLogLogPlus p k
insert e = insertHash (hash64 e)

insertHash :: forall p k . (KnownNat p, KnownNat k) => Hash64 -> HyperLogLogPlus p k -> HyperLogLogPlus p k
insertHash hash hll@(HyperLogLogPlus rank minSet) = HyperLogLogPlus rank' minSet'
  where p = fromIntegral $ pctx hll
        k = fromIntegral $ kctx hll
        idx = bucketIdx p hash
        rnk = calcRank p hash
        rank' = V.modify (\mv -> do
                  old <- MV.read mv idx
                  when (rnk > old) $ MV.write mv idx rnk
                ) rank
        minSet' | Set.size s > k = Set.deleteMax s
                | otherwise      = s
                where s = Set.insert hash minSet

size :: forall p k . (KnownNat p, KnownNat k) => HyperLogLogPlus p k -> Word64
size hll@(HyperLogLogPlus rank minSet)
  | ss < k    = fromIntegral ss
  | otherwise = round $ estimatedSize hll
  where k = fromIntegral $ kctx hll
        ss = Set.size minSet

-- | Compute estimted size based on HLL
estimatedSize :: forall p k . (KnownNat p, KnownNat k) => HyperLogLogPlus p k -> Double
estimatedSize hll@(HyperLogLogPlus rank minSet)
  | h <= thresholds ! idx = h
  | otherwise             = e
  where p = pctx hll
        idx = fromIntegral $ p - minP
        nb = numBuckets p
        q =  fromIntegral nb
        nz = fromIntegral . V.length . V.filter (==0) $ rank
        s = V.sum . V.map (\r -> 2.0 ^^ (negate r)) $ rank
        ae = (alpha nb) * (q ^ 2) * (1.0 / s)
        e | ae < 5 * q = ae - estimatedBias ae (fromIntegral p)
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

-- |   Returns an estimate of the size of the intersection
-- of the given HyperLogLogPlus objects
intersection :: forall p k . (KnownNat p, KnownNat k) => [HyperLogLogPlus p k] -> Word64
intersection hs
  | null hs                        = 0
  | any (\hll -> size hll == 0) hs = 0
  | otherwise                      = round $ ((fromIntegral r) / (fromIntegral n)) * (fromIntegral s)
    where k = natVal (Proxy :: Proxy k)
          u = Set.unions $ map hllMinSet hs
          s = size $ foldl1 (<>) hs
          n = min (fromIntegral k) (maximum $ map (Set.size . hllMinSet) hs)
          (_, r) = V.foldl f (u, 0 :: Int) $ V.enumFromN 0 n
          f :: (Set Hash64, Int) -> Int -> (Set Hash64, Int)
          f (s, r) i
            | inAll     = (s', r + 1)
            | otherwise = (s', r)
            where (l, s') = Set.deleteFindMin s
                  inAll = all (\hll -> Set.member l $ hllMinSet hll) hs


-- | Compute bucket index for given HLL precision level
bucketIdx :: Integer -> Hash64 -> Int
bucketIdx p h = fromIntegral $ shiftR (asWord64 h) (64 - fromIntegral p)

-- | Compute hash rank for given HLL precision level
calcRank :: Integer -> Hash64 -> Int8
calcRank p h = 1 + lz
  where lz = fromIntegral $ nlz $ shiftL (asWord64 h) $ fromIntegral p

kctx :: forall p k . (KnownNat k) => HyperLogLogPlus p k -> Integer
kctx _ = natVal (Proxy :: Proxy k)

pctx :: forall p k . (KnownNat p) => HyperLogLogPlus p k -> Integer
pctx _ = natVal (Proxy :: Proxy p)
