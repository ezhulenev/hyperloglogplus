# HyperLogLogPlus

[![Hackage](https://img.shields.io/hackage/v/hyperloglogplus.svg)](https://hackage.haskell.org/package/hyperloglogplus) [![Build Status](https://secure.travis-ci.org/ezhulenev/hyperloglogplus.png?branch=master)](http://travis-ci.org/ezhulenev/hyperloglogplus)

Haskell implementation of HyperLogLog++ with MinHash for efficient cardinality and intersection estimation
using constant space.

See original AdRoll paper for details: [HyperLogLog and MinHash](http://tech.adroll.com/media/hllminhash.pdf)

Also [AdRoll blog post](http://tech.adroll.com/blog/data/2013/07/10/hll-minhash.html)

```Haskell
-- Example:
:set -XDataKinds
:load Data.HyperLogLogPlus

type HLL = HyperLogLogPlus 12 8192

mempty :: HLL

size (foldr insert mempty [1 .. 75000] :: HLL)

size $ (foldr insert mempty [1 .. 5000] ::  HLL) <> (foldr insert mempty [3000 .. 10000] :: HLL)

intersection $ [ (foldr insert mempty [1 .. 15000] ::  HLL)
               , (foldr insert mempty [12000 .. 20000] :: HLL) ]
```

## Testing

```
stack test
```
