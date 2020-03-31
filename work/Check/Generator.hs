-- Problem is that we cannot test aribtrary data structures (eg. that the user has defined)
-- Solution is to come up with a generator datatype which the user can specify for custom types and these can then be tested
-- Two possiblilities of stateful vs stateless generator.

-- Do we need to support user defined data?
-- Need some way to guarantee that 't' can be stored in a register(s?), how to do for lists?

{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE BlockArguments         #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE NoImplicitPrelude      #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE DeriveAnyClass         #-}
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

instance {-# OVERLAPPABLE #-} (SizedBits a) => Generator a where
    initial = unpack (constant 0)
    next current = unpack $ (pack current) .+. 1
    isFinal current = (pack current) .==. ones



newtype RandBits a = SizedBits a deriving (Generic, Bits)
type RandBit = Bit

bitWidthToA :: Int -> Integer
bitWidthToA sizeOfA = (2 ^ (sizeOfA `div` 2) - 1) * 4 + 1

instance {-# OVERLAPPABLE #-} (SizedBits a) => Generator (RandBits a) where
  initial = unpack $ constant 0
  next current = let a = bitWidthToA $ valueOf @(SizeOf a)
    in unpack $ pack current .*. (constant a) .+. 1
  isFinal current = pack (initial :: RandBits a) .==. pack (next current)

instance {-# OVERLAPPABLE #-} KnownNat n => Generator (RandBit n) where
  initial = constant 0
  next current = let a = bitWidthToA $ valueOf @n
    in current .*. (constant a) .+. 1
  isFinal current = initial .==. next current
