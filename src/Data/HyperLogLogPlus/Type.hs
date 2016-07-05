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
  , cast
  ) where

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
import           Data.Bits                   (shiftL, shiftR)
import           Data.Bits.Extras            (nlz)
import           Data.Digest.Murmur64

import           GHC.TypeLits
import           GHC.Int

-- | HyperLogLogPlus cardinality estimation paired with MinHash for intersection estimation
--
--  * 'p' - precision of HLL structure
--
--  * 'k' - precision of MinHash structure (max size)
--
-- Create new counter:
--
-- >>> :set -XDataKinds
-- >>> :load Data.HyperLogLogPlus
-- >>> type HLL = HyperLogLogPlus 12 8192
-- >>> mempty :: HLL
-- HyperLogLogPlus [ p = 12 k = 8192 ] [ minSet size = 0 ]
--
-- HyperLogLogPlus and MinHash precisions are specified in a type.
-- HLL precision 'p' should be between 4 and 18, starting from 10
-- for good accuracy.
--
-- MinHash precision 'k' ideally should be greater or equal 8192
-- for decent intersection estimation.
--
--
-- Estimating number of unique items:
--
-- >>> size (foldr insert mempty [1 .. 75000] :: HLL)
-- 75090
--
-- Combine multiple counters:
--
-- >>> size $ (foldr insert mempty [1 .. 5000] ::  HLL) <> (foldr insert mempty [3000 .. 10000] :: HLL)
-- 10044
--
-- Compute estimated set intersection:
--
-- >>> intersection $ [(foldr insert mempty [1 .. 15000] ::  HLL), (foldr insert mempty [12000 .. 20000] :: HLL)]
-- 3100
data HyperLogLogPlus (p :: Nat) (k :: Nat) = HyperLogLogPlus
  { hllRank   :: V.Vector Int8
  , hllMinSet :: Set Hash64
  } deriving (Eq)

type role HyperLogLogPlus nominal nominal

instance (KnownNat k) => Semigroup (HyperLogLogPlus p k) where
  (HyperLogLogPlus ar ah) <> (HyperLogLogPlus br bh) = HyperLogLogPlus (V.zipWith max ar br) (iterate Set.deleteMax u !! n)
    where k = fromIntegral $ natVal (Proxy :: Proxy k)
          u = Set.union ah bh
          n = max 0 (Set.size u - k)

instance (KnownNat p, KnownNat k, 4 <= p, p <= 18) => Monoid (HyperLogLogPlus p k) where
  mempty = HyperLogLogPlus (V.replicate (numBuckets p) 0) Set.empty
    where p = natVal (Proxy :: Proxy p)
  mappend = (<>)

instance (KnownNat p, KnownNat k) => Show (HyperLogLogPlus p k) where
  show hll@(HyperLogLogPlus _ minSet) = "HyperLogLogPlus [p = " ++ p ++ " k = " ++ k ++ " ] [ minSet size = " ++ s ++ " ]"
    where p = show $ pctx hll
          k = show $ kctx hll
          s = show $ Set.size minSet

-- | Insert hashable value
insert :: forall p k a . (KnownNat p, KnownNat k, Hashable64 a) => a -> HyperLogLogPlus p k -> HyperLogLogPlus p k
insert e = insertHash (hash64 e)

-- | Insert already hashed value
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

-- | Compute estimated size of HyperLogLogPlus. If number of inserted values is smaller than
-- MinHash precision this will return exact value
size :: forall p k . (KnownNat p, KnownNat k) => HyperLogLogPlus p k -> Word64
size hll@(HyperLogLogPlus _ minSet)
  | ss < k    = fromIntegral ss
  | otherwise = round $ estimatedSize hll
  where k = fromIntegral $ kctx hll
        ss = Set.size minSet

-- | Compute estimted size based on HLL
estimatedSize :: forall p k . (KnownNat p, KnownNat k) => HyperLogLogPlus p k -> Double
estimatedSize hll@(HyperLogLogPlus rank _)
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

-- | Returns an estimate of the size of the intersection
-- of the given HyperLogLogPlus objects
intersection :: forall p k . (KnownNat p, KnownNat k) => [HyperLogLogPlus p k] -> Word64
intersection hs
  | null hs                        = 0
  | any (\hll -> size hll == 0) hs = 0
  | otherwise                      = round $ ((fromIntegral r) / (fromIntegral n)) * (fromIntegral ts)
    where k = natVal (Proxy :: Proxy k)
          u = Set.unions $ map hllMinSet hs
          ts = size $ foldl1 (<>) hs
          n = min (fromIntegral k) (maximum $ map (Set.size . hllMinSet) hs)
          (_, r) = V.foldl f (u, 0 :: Int) $ V.enumFromN 0 n
          f :: (Set Hash64, Int) -> Int -> (Set Hash64, Int)
          f (s, cnt) _
            | inAll     = (s', cnt + 1)
            | otherwise = (s', cnt)
            where (l, s') = Set.deleteFindMin s
                  inAll = all (\hll -> Set.member l $ hllMinSet hll) hs

-- | Cast HyperLogLogPlus to new precision levels
--
--  1. New HLL precision should less or equal to old one
--  2. New MinHash precision has to be less or equal to old one,
--  or it can be larger, but only if number of inserted hashes in old
--  structure is smaller than old precision (size limit)
cast :: forall p1 k1 p2 k2 . (KnownNat p1, KnownNat k1, KnownNat p2, KnownNat k2,
                              4 <= p2, p2 <= 18)
     => HyperLogLogPlus p1 k1 -> Maybe (HyperLogLogPlus p2 k2)
cast oldHll
  -- shrinking HLL and MinHash precision
  | p2 <= p1 && k2 <= k1 = Just $ HyperLogLogPlus rank minSet
  -- shrinking HLL precision and extending MinHash precision
  -- only if observed hashes are smaller than old precision
  | p2 <= p1 &&
    (k2 > k1 && sz < k1) = Just $ HyperLogLogPlus rank minSet
  -- othersize cast is not possible
  | otherwise            = Nothing
  where
    newHll = mempty :: HyperLogLogPlus p2 k2
    p1 = pctx oldHll
    k1 = kctx oldHll
    p2 = pctx newHll
    k2 = kctx newHll
    sz = fromIntegral $ Set.size $ hllMinSet oldHll
    newBuckets = numBuckets p2
    -- compute new ranks
    rank = V.modify (\m ->
        V.forM_ (V.indexed $ hllRank oldHll) $ \ (i, o) -> do
          let j = mod i newBuckets
          a <- MV.read m j
          MV.write m j (max o a)
      ) $ hllRank newHll
    -- delete hashes from min-set if required
    ndel = max 0 (sz - k2)
    minSet = iterate Set.deleteMax (hllMinSet oldHll) !! (fromIntegral ndel)

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
