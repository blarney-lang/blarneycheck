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

module Utils where

import Blarney
import Generator
import Property

doActionList :: [Action ()] -> Action()
doActionList as = foldr (>>) noAction as

incrementGenList :: Generator a => [a] -> [a]
incrementGenList as = igl 1 as
  where igl :: Generator a => Bit 1 -> [a] -> [a]
        igl _ [] = []
        igl inc (x:xs) =
          let incXs = igl (inc .&. isFinal x) xs
          in (unpack $ mux inc (mux (isFinal x) (initial, pack (next x)), pack x)):incXs

isPureProp :: Prop -> Bool
isPureProp (WhenAction _ _) = False
isPureProp (WhenRecipe _ _) = False
isPureProp (Assert _) = True
isPureProp (Forall f) = isPureProp (f undefined)
isPureProp (ForallList _ f) = isPureProp (f undefined)

splitProperties :: [Property] -> ([Property], [Property])
splitProperties [] = ([], [])
splitProperties (property@(_, prop):props) = 
  let (pures, impures) = splitProperties props
  in if (isPureProp prop) then (property:pures, impures) else (pures, property:impures)
