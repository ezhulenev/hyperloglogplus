{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RoleAnnotations     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.HyperLogLogPlus.Type
  (
    HyperLogLogPlus()
  ) where

import           Data.HyperLogLogPlus.Config

import           Data.Set                    (Set)
import qualified Data.Set                    as Set

import qualified Data.Vector.Unboxed         as V
import qualified Data.Vector.Unboxed.Mutable as MV

import           Data.Proxy
import           Data.Semigroup

import           GHC.TypeLits

-- Example:
-- >>> :set -XDataKinds
-- >>> :load Data.HyperLogLogPlus
-- >>> mempty :: HyperLogLogPlus 5 5

data HyperLogLogPlus (p :: Nat) (k :: Nat) = HyperLogLogPlus
  { hllRank   :: V.Vector Rank
  , hllMinSet :: Set Hash
  } deriving (Eq, Show)

type role HyperLogLogPlus nominal nominal

instance Semigroup (HyperLogLogPlus p k) where
  HyperLogLogPlus lrnk lset <> HyperLogLogPlus rrnk rset = undefined

instance (KnownNat p, KnownNat k) => Monoid (HyperLogLogPlus p k) where
  mempty = HyperLogLogPlus (V.replicate p' 0) Set.empty
    where p' = fromInteger $ natVal (Proxy :: Proxy p) :: Int
  mappend = (<>)

