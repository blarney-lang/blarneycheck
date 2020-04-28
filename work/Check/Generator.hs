-- Problem is that we cannot test aribtrary data structures (eg. that the user has defined)
-- Solution is to come up with a generator datatype which the user can specify for custom types and these can then be tested
-- Two possiblilities of stateful vs stateless generator.

-- Do we need to support user defined data?
-- Need some way to guarantee that 't' can be stored in a register(s?), how to do for lists?

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE GADTs #-}

module Check.Generator where

import Blarney

type SizedBits a = (Bits a, KnownNat (SizeOf a))

class SizedBits a => Generator a where
  initial :: a
  next :: a -> a
  isFinal :: a -> (Bit 1)
  -- Only used for calculating cycles required for testing, does not need to be defined:
  range :: forall a'. (a' ~ a) => Integer

instance {-# OVERLAPPABLE #-} (SizedBits a) => Generator a where
  initial = unpack (constant 0)
  next current = unpack $ (pack current) .+. 1
  isFinal current = (pack current) .==. ones
  range = 2^valueOf @(SizeOf a)


-- For "RandBits (Bit n) (Seed i s)":
--  Nat 'i' is initial value
--  Nat 's' is seed for ordering values
-- In general there are 2^(n-2)-1 possible orderings thus 0 <= 's' < 2^(n-2)-1
-- For larger 's' >= 2^(n-2)-1 + m, with m >= 0, ordering will be same as if 's' = m
-- Note that, although pseudo-random, the order of the lowest 4 bits is rarely affected by 's' values
-- Also note that when 's' = 2^(n-2)-n, the order is the same as the normal generator (next === +1)
newtype RandBits a s = RandBits a deriving (Generic, Bits)

data Seed (i :: Nat) (s :: Nat)

seedToA :: forall n s. (KnownNat n, KnownNat s) => Integer
seedToA = seed * 4 + 1
  where seed = toInteger ((valueOf @s + valueOf @n - 1) `mod` (2 ^ (max 2 $ valueOf @n - 2) - 1))

instance {-# OVERLAPPABLE #-} (SizedBits b, KnownNat i, KnownNat s) => Generator (RandBits b (Seed i s)) where
  initial = unpack $ constant $ toInteger (valueOf @i) `mod` (2 ^ valueOf @(SizeOf b))
  next current = let a = seedToA @(SizeOf b) @s
    in unpack $ pack current .*. (constant a) .+. 1
  isFinal current = pack (initial :: RandBits b (Seed i s)) .==. pack (next current)
  range = 2^valueOf @(SizeOf b)
  