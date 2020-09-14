module BlarneyCheck.Check
  ( checkPure
  , checkPureFPGA
  , check
  , checkFPGA
  , estimateTestCaseCount
  ) where

import Blarney
import Blarney.Queue
import BlarneyCheck.Property
import BlarneyCheck.PureProp
import BlarneyCheck.ImpureProp
import BlarneyCheck.TestBench
import BlarneyCheck.Utils

-- Use this function for simulation
checkPure :: [Property] -> Module (Bit 1, Bit 1)
checkPure = checkPure' (testPlusArgs "DEBUG")

-- Use this function for synthesis
checkPureFPGA :: Queue (Bit 8) -> [Property] -> Module ()
checkPureFPGA bytesOut props = checkPure' 0 props >>= outputForFPGA bytesOut

-- Check for pure only properties
checkPure' :: Bit 1 -> [Property] -> Module (Bit 1, Bit 1)
checkPure' isDebug props = let (pureProps, impureProps) = splitProperties props in
  if (length impureProps /= 0) then error "Trying to check impure properties without specifying a valid depth!" else do
    pureTB <- combinePureProps pureProps
    globalTime :: Reg (Bit 32) <- makeReg 0
    allDone :: Reg (Bit 1) <- makeReg 0
    always do
      globalTime <== globalTime.val + 1
      when (pureTB.failed.inv) do
        when isDebug do display_ "Pass: " >> displayFail pureTB >> display_ "\n"
        when (pureTB.isDone) do
          _ <- display "--All tests passed at time %0d" (globalTime.val) "--"
          allDone <== 1
          finish
        increment pureTB
      when (pureTB.failed) do
        _ <- display "@ Fail at time %0d" (globalTime.val) " @"
        displayFail pureTB >> display_ "\n"
        allDone <== 1
        finish
    return (allDone.val, pureTB.failed)

-- Use this function for simulation
check :: [Property] -> Action() -> Int -> Module (Bit 1, Bit 1)
check = check' (testPlusArgs "DEBUG")

-- Use this function for synthesis
checkFPGA :: Queue (Bit 8) -> [Property] -> Action() -> Int -> Module ()
checkFPGA bytesOut props rst maxSeqLen = check' 0 props rst maxSeqLen >>= outputForFPGA bytesOut

-- Check for sequential modules
check' :: Bit 1 -> [Property] -> Action() -> Int -> Module (Bit 1, Bit 1)
check' isDebug props rst maxSeqLen = let (pureProps, impureProps) = splitProperties props in
  if (maxSeqLen <= 0 || length impureProps == 0) then checkPure' isDebug props else do
  disableDebug :: Wire (Bit 1) <- makeWire 1

  purePhaseReg :: Reg (Bit 1) <- makeReg 1
  displayingFail :: Reg (Bit 1) <- makeReg 0
  allDone :: Reg (Bit 1) <- makeReg 0
  globalTime :: Reg (Bit 32) <- makeReg 0
  failTime :: Reg (Bit 32) <- makeReg 0
  pureTB <- combinePureProps pureProps

  stateTester <- makeStatefulTester impureProps rst maxSeqLen (displayingFail.val) isDebug
  let startPureTests = stateTester.sequenceDone .&. stateTester.finishedExec
  let purePhase = purePhaseReg.val .|. startPureTests

  always do
    if (allDone.val) then finish else do
    if (displayingFail.val) then do
      failTime <== failTime.val + 1
      display_ "%0d: " (failTime.val)
      if (stateTester.sequenceDone .&. stateTester.finishedExec) then do
        -- Check that pure test bench still fails after replaying sequence
        if (pureTB.failed) then (display_ "'" >> displayFail pureTB >> display "' fails" :: Action ())
        else (display "Failure was not observed when replaying sequence! Reset may be incorrect")
        allDone <== 1
      else do
        when (stateTester.finishedExec) do execImpureEdge stateTester
        when (stateTester.finishedExec.inv) do display_ "(Waiting for Recipe)"
        display_ "\n"
    else do
      if purePhase then do
        if (pureTB.failed) then do
          when isDebug do display_ "\n"
          display_ "=== Found failing case at depth " >> displaySeqLen stateTester
          display_ " after %0d ticks" (globalTime.val) ": " >> displayFail pureTB >> display " ==="
          reset stateTester
          displayingFail <== 1
        else do
          when (isDebug) do 
            disableDebug <== 0
            display_ "Pass: " >> displayFail pureTB
          increment pureTB -- Implicitly resets when isDone
          purePhaseReg <== pureTB.isDone.inv
          when (pureTB.isDone) do reset stateTester
          when (pureTB.isDone .&. stateTester.allSeqExec) do
            when (isDebug) do
              disableDebug <== 1
              display_ "\n"
            if (stateTester.atMaxSeqLen) then do
              display_ "=== All tests passed to maximum specified depth of " >> displaySeqLen stateTester
              display " at time %0d" (globalTime.val) " ==="
              allDone <== 1
            else do
              incSeqLen stateTester
              display " at time %0d" (globalTime.val) " -"
      else do
        when (isDebug) do
          disableDebug <== 0
          when (stateTester.finishedExec.inv) do display_ "(Waiting)"
        execImpureEdge stateTester
  
  always do
    when (isDebug .&. disableDebug.val.inv) do
      display_ "\t| "
      when (purePhase .&. pureTB.isDone) do display_ "\n"
    globalTime <== globalTime.val + 1
  return (allDone.val, displayingFail.val)

-- Util for static analysis
estimateTestCaseCount :: [Property] -> Integer -> Module ()
estimateTestCaseCount props maxSeqLen =
  let (pureProps, impureProps) = splitProperties props
      simPerSec = if (length impureProps == 0) then 10^7 else 2*10^6
      synthPerSec = 5*10^7
      pureCases = foldl1 (max) $ map (\(_, p) -> getCases p) pureProps
      impureCases = foldl (+) 0 $ map (\(_, p) -> getCases p) impureProps
      depthClocks = scanl1 (+) [(d + pureCases) * (impureCases ^ d) | d <- [0 .. maxSeqLen]]
      depthTimings = map (\clks -> (clocksToTimes clks simPerSec, clocksToTimes clks synthPerSec)) depthClocks
      depthInfos = zip3 ([0 ..] :: [Integer]) depthClocks depthTimings
  in do
    always do
      display_ "- If sim runs at " >> displayClkFreq simPerSec >> display_ " (" >> displayBits simPerSec
      display_ ") and native at " >> displayClkFreq synthPerSec >> display_ " (" >> displayBits synthPerSec >> display ") -"
      if (containsWhenRecipe impureProps) then display "- Assuming that WhenRecipes only take one cycle -" else noAction
      foldl1 (>>) $ map displayInfo depthInfos
      finish
  where displayInfo (d, count, (simTime, synthTime)) = do
          display_ "Clock cycles to test to depth " d ": " count " or " >> displayBits count
          display_ "\nSimulation would run for about: " >> displayTime simTime
          display_ "\nSynthesized testing would take: " >> displayTime synthTime
          display "\n"
