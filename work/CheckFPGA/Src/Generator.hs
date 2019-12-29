-- Problem is that we cannot test aribtrary data structures (eg. that the user has generated)
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
{-# LANGUAGE IncoherentInstances #-}

module CheckFPGA.Src.Generator where

import Blarney

type SizedBits a = (Bits a, KnownNat (SizeOf a))

class Generator t where
    initial :: t
    next :: t -> t
    isLast :: t -> Bit 1

instance KnownNat n => Generator (Bit n) where
    initial = constant 0
    next = \prev -> prev .+. 1
    isLast = \value -> value .==. ones

instance (SizedBits a) => Generator a where
    initial = unpack (constant 0)
    next = \prev -> unpack $ (pack prev) .+. 1
    isLast = \value -> (pack value) .==. ones


{-
data GeneratorState t = GeneratorState 
    { peek :: t
    , isLast :: Bit 1
    , reset :: Action ()
    , consume :: Action ()
    } deriving (Generic, Interface)


bitGen :: Bits a => Module (GeneratorState a)
bitGen = do

    Generator {
    peek = constant 0,
    next = \prev -> prev .+. 1,
    isLast = \value -> value .==. ones
}
-}