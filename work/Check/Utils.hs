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

module Check.Utils where

import Blarney
import Check.Generator
import Check.Property

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
isPureProp (ForallList _ f) = isPureProp (f [undefined])

splitProperties :: [Property] -> ([Property], [Property])
splitProperties [] = ([], [])
splitProperties (property@(_, prop):props) = 
  let (pures, impures) = splitProperties props
  in if (isPureProp prop) then (property:pures, impures) else (pures, property:impures)


getCases :: Prop -> Integer
getCases (WhenAction _ _) = 1
getCases (WhenRecipe _ _) = 1
getCases (Assert _) = 1
getCases (Forall (f :: a -> Prop)) = getCases (f undefined) * (range @a)
getCases (ForallList n (f :: [a] -> Prop)) = getCases (f [undefined]) * ((range @a) ^ n)

containsWhenRecipe :: [Property] -> Bool
containsWhenRecipe props = foldl (||) (False) $ map (\(_, p) -> isWhenRecipe p) props
  where isWhenRecipe (WhenAction _ _) = False
        isWhenRecipe (WhenRecipe _ _) = True
        isWhenRecipe (Assert _) = False
        isWhenRecipe (Forall f) = isWhenRecipe (f undefined)
        isWhenRecipe (ForallList _ f) = isWhenRecipe (f [undefined])

type Time = (Integer, Integer, Integer, Integer)
clocksToTimes :: Integer -> Integer -> Time
clocksToTimes clks perSecond = (miliseconds `div` (36*10^5), (miliseconds `div` 60000) `rem` 60, (miliseconds `div` 1000) `rem` 60, miliseconds `rem` 1000)
  where miliseconds = (clks * 1000) `div` perSecond

displayTime :: Time -> Action ()
displayTime (hrs, mins, secs, milis) = do
  if (hrs /= 0) then display_ hrs "h " else noAction
  if (mins /= 0) then display_ mins "m " else noAction
  display_ secs "." milis "s"

displayClkFreq :: Integer -> Action ()
displayClkFreq freq =
  if (freq `rem` 10^3 /= 0) then display_ freq "Hz" else
  if (freq `rem` 10^6 /= 0) then display_ (freq `div` 10^3) "kHz" else
  display_ (freq `div` 10^6) "MHz"