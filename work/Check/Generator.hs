-- Problem is that we cannot test aribtrary data structures (eg. that the user has generated)
-- Solution is to come up with a generator datatype which the user can specify for custom types and these can then be tested
-- Two possiblilities of stateful vs stateless generator.

{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE BlockArguments         #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE NoImplicitPrelude      #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE DeriveAnyClass         #-}
{-# LANGUAGE ConstraintKinds #-}

module Check.Generator where

import Blarney

type SizedBits a = (Bits a, KnownNat (SizeOf a))

data Generator t = Generator 
    { initial :: t
    , next :: t -> t
    , isLast :: t -> Bit 1
    }

bitGen :: KnownNat n => Generator (Bit n)
bitGen = Generator {
    initial = constant 0,
    next = \prev -> prev .+. 1,
    isLast = \value -> value .==. ones
}

bitsGen :: (Bits a, KnownNat (SizeOf a)) => Generator a
bitsGen = Generator {
    initial = unpack (constant 0),
    next = \prev -> unpack $ (pack prev) .+. 1,
    isLast = \value -> (pack value) .==. ones
}

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