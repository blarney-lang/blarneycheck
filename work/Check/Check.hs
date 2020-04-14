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

check :: Action() -> [Property] -> Int -> Module(Bit 1)
check rst props depth = do
  let (asserts, sideEffects) = splitProperties props
  pureTB <- combinePureProps asserts
  impureTB <- makeImpureTestBench depth rst sideEffects

  let purePhaseDone = pureTB.isDone .&. pureTB.failed.inv
  globalTime :: Reg (Bit 32) <- makeReg 0
  purePhase :: Reg (Bit 1) <- makeReg 1
  displayingFail :: Reg (Bit 1) <- makeReg 0
  allDone :: Reg (Bit 1) <- makeReg 0
  _ <- always do
    if (displayingFail.val) then do
      impureTB.displayFailImpure
      when (impureTB.depthDone) finish
    else do
      when (purePhase.val.inv .|. purePhaseDone) do
        if (impureTB.depthDone) then do
          _ <- display "--All tests passed to depth %0d" (impureTB.currMaxDepth) " at time %0d" (globalTime.val) "--"
          if ((impureTB.currMaxDepth) .>=. (constant (toInteger depth))) then do
            allDone <== 1
            finish
          else do
            impureTB.incMaxDepth
        else do
          impureTB.runEdge
          purePhase <== impureTB.edgeDone
          --display "-ImpureEdge"

      when (purePhase.val .|. impureTB.edgeDone) do
        pureTB.increment
        purePhase <== purePhaseDone.inv
        when (pureTB.isDone) (pureTB.reset)
        when (pureTB.failed) do
          _ <- display "@ Fail at time %0d" (globalTime.val) " @"
          pureTB.displayFailPure
          displayingFail <== 1
        --display "+PureCheck"
      --when (globalTime.val .>. 600) finish
      globalTime <== globalTime.val + 1
      --display "------TICK------"
      --display "Time: " (globalTime.val)
      --when (tb.isDone) do
        --display "Test pass"
  return (allDone.val)


  
  {-
  let propSorted = Pure "Sorted" (5 :: Int, \(xs :: [Bit 2]) -> isSorted (sort xs))

  let rst = noAction
  _ <- check rst [propSorted] 10
  -}
  {-
  stackSpec :: Stack (Bit 3) <- makeStackSpec 10
  stack :: Stack (Bit 3) <- makeStack 10

  let stackPush = Impure "Push" \x -> (1 :: Bit 1, Seq [Action $ (stackSpec.push) x, Action $ (stack.push) x])
  let stackPop =  Impure "Pop" (stackSpec.isEmpty.inv .&. stack.isEmpty.inv, (stackSpec.pop) >> (stack.pop))
  --let stackPushPopNop =  Impure "Pop" \x -> (stack.isEmpty.inv, (stack.push) x >> (stack.pop))

  let propStackTopEq = Pure "StackTopEq" (stackSpec.isEmpty .|. (stackSpec.top .==. stack.top))

  let rst = (stackSpec.clear) >> (stack.clear)
  _ <- check rst [propStackTopEq, stackPush, stackPop] 6
  -}

  --always do
    --when done
      --finish
      
  {-
  --queueOfEdgesTaken :: Queue (Bit 16) <- makeSizedQueue 3
  recStart :: Reg (Bit 1) <- makeReg 0
  let recipe = 
        Seq [
          Action do
            display "Rec 0"
        , Action do
            display "Rec 1"
        , Action do
            display "Rec 2"
        ]
  recipeEnd <- run (recStart.val) recipe
  let test =
        Seq [
          Action do
            recStart <== 1
        , Action do
            recStart <== 0
            display recipeEnd
        , Action do
            display recipeEnd
        , Action do
            display recipeEnd
        , Action do
            display recipeEnd
        , Action do
            display recipeEnd
        , Action do
            display recipeEnd
        , Action do
            display recipeEnd
        , Action do
            display recipeEnd
        , Action do
            noAction
        , Action do
            finish
        ]
  runOnce test
  -}