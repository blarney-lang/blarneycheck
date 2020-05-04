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
import Blarney.Queue
import Check.Generator
import Check.Property

import Data.Char (ord)

charToByte :: Char -> Bit 8
charToByte ch = constant (toInteger (ord ch))

outputForFPGA :: Queue (Bit 8) -> (Bit 1, Bit 1) -> Module ()
outputForFPGA bytesOut (done, testFailed) = do
  stop :: Reg (Bit 1) <- makeReg 0
  always do
    when (stop.val.inv) do
      when (testFailed) do
        enq bytesOut (charToByte 'F')
        stop <== 1
      when (done) do
        enq bytesOut (charToByte 'P')
        stop <== 1


isPureProp :: Prop -> Bool
isPureProp (WhenAction _ _) = False
isPureProp (WhenRecipe _ _) = False
isPureProp (Assert _) = True
isPureProp (Assert' _ _) = True
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
getCases (Assert' _ _) = 1
getCases (Forall (f :: a -> Prop)) = getCases (f undefined) * (range @a)
getCases (ForallList n (f :: [a] -> Prop)) = getCases (f [undefined]) * ((range @a) ^ n)

containsWhenRecipe :: [Property] -> Bool
containsWhenRecipe props = foldl (||) (False) $ map (\(_, p) -> isWhenRecipe p) props
  where isWhenRecipe (WhenAction _ _) = False
        isWhenRecipe (WhenRecipe _ _) = True
        isWhenRecipe (Assert _) = False
        isWhenRecipe (Assert' _ _) = False
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
  display_ secs "." (replicate (3 - length (show milis)) '0') milis "s"

displayClkFreq :: Integer -> Action ()
displayClkFreq freq =
  if (freq `rem` 10^3 /= 0) then display_ freq "Hz" else
  if (freq `rem` 10^6 /= 0) then display_ (freq `div` 10^3) "kHz" else
  display_ (freq `div` 10^6) "MHz"

displayBits :: Integer -> Action ()
displayBits cycles = 
  let bits = cycles.fromInteger.(logBase 2.0).(* 10).round.toInteger
  in display_ (bits `div` 10) "." (bits `mod` 10) " bits"
