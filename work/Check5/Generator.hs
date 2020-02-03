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

module Check5.Generator where

import Blarney

type SizedBits a = (Bits a, KnownNat (SizeOf a))

class SizedBits a => Generator a where
    initial :: a
    next :: a -> a
    isFinal :: a -> (Bit 1)

instance (SizedBits a) => Generator a where
    initial = unpack (constant 0)
    next current = unpack $ (pack current) .+. 1
    isFinal current = (pack current) .==. ones


doActionList :: [Action ()] -> Action()
doActionList as = foldr (>>) noAction as

incrementGenList :: Generator a => [a] -> [a]
incrementGenList as = igl 1 as
  where igl :: Generator a => Bit 1 -> [a] -> [a]
        igl _ [] = []
        igl inc (x:xs) =
          let incXs = igl (inc .&. isFinal x) xs
          in (unpack $ mux inc (mux (isFinal x) (initial, pack (next x)), pack x)):incXs