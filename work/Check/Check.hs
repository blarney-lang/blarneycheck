{-# LANGUAGE GADTs #-}

module Check.Check (
  module Check.Generator
, module Check.Property
, module Check.Check
) where

import Blarney
import Check.Generator
import Check.Property
import Check.PureProp
import Check.ImpureProp
import Check.TestBench
import Check.Utils


checkPure :: [Property] -> Module (Bit 1)
checkPure props = let (asserts, sideEffects) = splitProperties props in
  if (length sideEffects /= 0) then error "Trying to check impure properties without specifying a valid depth!" else do
    pureTB <- combinePureProps asserts
    globalTime :: Reg (Bit 32) <- makeReg 0
    allDone :: Reg (Bit 1) <- makeReg 0
    always do
      globalTime <== globalTime.val + 1
      when (pureTB.failed.inv) do
        when (pureTB.isDone) do
          _ <- display "--All tests passed at time %0d" (globalTime.val) "--"
          allDone <== 1
          finish
        pureTB.increment
      when (pureTB.failed) do
        _ <- display "@ Fail at time %0d" (globalTime.val) " @"
        pureTB.displayFailPure
        finish
    return (allDone.val)


check :: Action() -> [Property] -> Int -> Module (Bit 1)
check rst props depth = if (depth <= 0) then checkPure props else do
  let (asserts, sideEffects) = splitProperties props
  pureTB <- combinePureProps asserts
  impureTB <- makeImpureTestBench depth rst sideEffects

  let fail = pureTB.failed
  let purePhaseDone = pureTB.isDone .&. fail.inv
  globalTime :: Reg (Bit 32) <- makeReg 0
  purePhase :: Wire (Bit 1) <- makeWire 1
  displayingFail :: Reg (Bit 1) <- makeReg 0
  allDone :: Reg (Bit 1) <- makeReg 0
  _ <- always do
    if (displayingFail.val) then do
      impureTB.displayFailImpure
      when (impureTB.depthDone) finish -- Maybe check here that fail is true again, if not may have been a problem with rst instead!
    else do
      purePhase <== purePhase.val.(delay 0) ? (purePhaseDone.inv, impureTB.edgeDone)
      when (purePhase.val.inv) do -- TODO: Run this when in increment phase during pure checks
        if (impureTB.depthDone) then do
          _ <- display "--All tests passed to depth %0d" (impureTB.currMaxDepth) " at time %0d" (globalTime.val) "--"
          if ((impureTB.currMaxDepth) .>=. (constant (toInteger depth))) then do
            allDone <== 1
            finish
          else do
            impureTB.incMaxDepth
        else do
          impureTB.runEdge
          --display "-ImpureEdge"

      when (purePhase.val .|. pureTB.isDone) do
        --display "-- Tick --"
        when (fail.inv) do
          pureTB.increment -- Implicitly resets when isDone
        when (fail) do
          _ <- display "@ Fail at time %0d" (globalTime.val) " @"
          pureTB.displayFailPure
          displayingFail <== 1
        --display "+PureCheck"
      globalTime <== globalTime.val + 1
      --display "------TICK------"
      --display "Time: " (globalTime.val)
  return (allDone.val)