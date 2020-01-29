{-# LANGUAGE GADTs #-}

import Blarney
import Check5.Property
import Check5.Stack
import Check5.StackGolden

--import Blarney.Queue


check :: Action() -> [Prop] -> Int -> Module(Bit 1)
check rst props depth = do
  let (asserts, sideEffects) = splitProps props
  pureTB <- combinePureTestBenches $ mapM checkGenPure asserts
  impureTB <- makeImpureTestBench depth rst sideEffects

  globalTime :: Reg (Bit 32) <- makeReg 0
  pureTestsDone :: Reg (Bit 1) <- makeReg 0
  displayingFail :: Reg (Bit 1) <- makeReg 0
  allDone :: Reg (Bit 1) <- makeReg 0
  _ <- always do
    if (displayingFail.val) then do
      impureTB.displayFail
      when (impureTB.depthDone) finish
    else do
      if(impureTB.edgeDone.inv .|. pureTestsDone.val) then do
        if (impureTB.depthDone) then do
          _ <- display "--All tests passed to depth " (impureTB.currMaxDepth) "--"
          if ((impureTB.currMaxDepth) .>=. (constant (toInteger depth))) then do
            allDone <== 1
            finish
          else do
            impureTB.incMaxDepth
        else do
          impureTB.runEdge
          pureTestsDone <== 0
          display "-ImpureEdge"
      else do
        pureTB.runTest
        pureTB.increment
        pureTestsDone <== pureTB.isDone
        when (pureTB.isDone) (pureTB.reset)
        when (pureTB.failed) do
          displayingFail <== 1
        display "+PureCheck"
        
      globalTime <== globalTime.val + 1
      display "------TICK------"
      --display "Time: " (globalTime.val)
      --when (tb.isDone) do
        --display "Test pass"
  return (allDone.val)



twoSort :: KnownNat n => (Bit n, Bit n) -> (Bit n, Bit n)
twoSort (a :: Bit n, b) = let halfSize = constant (toInteger (valueOf @(n))) in
                          (b - a) + halfSize .>=. halfSize ? ((a, b), (b, a))
--twoSort (a, b) = a .<. b ? ((a, b), (b, a))

bubble :: KnownNat n => [Bit n] -> [Bit n]
bubble [] = []
bubble [x] = [x]
bubble (x:y:rest) = bubble (small:rest) ++ [big]
  where (small, big) = twoSort (x, y)

sort :: KnownNat n => [Bit n] -> [Bit n]
sort [] = []
sort (x:xs) = smallest : sort rest
  where (smallest:rest) = bubble (x:xs)

isSorted :: KnownNat n => [Bit n] -> Bit 1
isSorted [] = 1
isSorted [_] = 1
isSorted (x1:x2:xs) = (x1 .<=. x2) .&. isSorted (x2:xs)



top :: Module ()
top = do
  stackGold :: Check5.StackGolden.Stack 10 (Bit 3) <- Check5.StackGolden.makeStack
  stackGold2 :: Check5.StackGolden.Stack 10 (Bit 3) <- Check5.StackGolden.makeStack
  stack :: Check5.Stack.Stack (Bit 3) <- Check5.Stack.makeStack 10
  let stackPush = Forall "X" \x -> (WhenAction "Push" (1) ((stackGold.push1) x >> (stackGold2.push1) x >> (stack.push) x)) -- (stack1.overflow.inv) .&. (stack2.overflow.inv)
  let stackPop = WhenAction "Pop" (1) ((stackGold.Check5.StackGolden.pop) 1 >> (stackGold2.Check5.StackGolden.pop) 1 >> (stack.Check5.Stack.pop)) -- (stack1.underflow.inv) .&. (stack2.underflow.inv)
  --let propSTS = Forall "Y" \(y :: Bit 2) -> Assert "Stack 2 top small" (stack1.top1 - stack2.top2 .<. 7)

  --let propSTSS = Assert "Stack top sum small" ((zeroExtend $ stack1.top2) + (zeroExtend $ stack1.top1) .<. (constant 14 :: Bit 4))
  --let propStackTop1Eq = Assert "Stack top equal" (stackGold.top1 .==. stack.Check5.Stack.top) (display_ "Gold top " (stackGold.top1) ", " >> (display_ "Other top " (stack.Check5.Stack.top) ", " ))
  let propStackTop2Eq = Assert "Stack top2 equal" (stackGold.top1 .==. stackGold2.top1) noAction

  done <- check ((stackGold.Check5.StackGolden.pop) (stackGold.size) >> (stack.clear)) [propStackTop2Eq, stackPush, stackPop] 3
  
  --always do
    --when done
      --finish
      
  {-    
  queueOfEdgesTaken :: Queue (Bit 16) <- makeSizedQueue 3
  let test =
        Seq [
          Action do
            (queueOfEdgesTaken.enq) 8
        , Action do
            --(queueOfEdgesTaken.enq) ((queueOfEdgesTaken.first) + 1)
            queueOfEdgesTaken.deq
            display (queueOfEdgesTaken.first)
        , Action do
            queueOfEdgesTaken.deq
            (queueOfEdgesTaken.enq) ((queueOfEdgesTaken.first) + 1)
            display (queueOfEdgesTaken.first)
        , Action do
            queueOfEdgesTaken.deq
            (queueOfEdgesTaken.enq) ((queueOfEdgesTaken.first) + 1)
            display (queueOfEdgesTaken.first)
        , Action do
            queueOfEdgesTaken.deq
            (queueOfEdgesTaken.enq) ((queueOfEdgesTaken.first) + 1)
            display (queueOfEdgesTaken.first)
        , Action do
            queueOfEdgesTaken.deq
            (queueOfEdgesTaken.enq) 0
            display (queueOfEdgesTaken.first)
        , Action do
            queueOfEdgesTaken.deq
            (queueOfEdgesTaken.enq) ((queueOfEdgesTaken.first) + 1)
            display (queueOfEdgesTaken.first)
        , Action do
            queueOfEdgesTaken.deq
            (queueOfEdgesTaken.enq) ((queueOfEdgesTaken.first) + 1)
            display (queueOfEdgesTaken.first)
        , Action do
            queueOfEdgesTaken.deq
            (queueOfEdgesTaken.enq) ((queueOfEdgesTaken.first) + 1)
            display (queueOfEdgesTaken.first)
        , Action do
            queueOfEdgesTaken.deq
            (queueOfEdgesTaken.enq) ((queueOfEdgesTaken.first) + 1)
            display (queueOfEdgesTaken.first)
        , Action do
            queueOfEdgesTaken.deq
            (queueOfEdgesTaken.enq) ((queueOfEdgesTaken.first) + 1)
            display (queueOfEdgesTaken.first)
        , Action do
            queueOfEdgesTaken.deq
            (queueOfEdgesTaken.enq) ((queueOfEdgesTaken.first) + 1)
            display (queueOfEdgesTaken.first)
        , Action do
            finish
        ]

  runOnce test-}
  return ()


main :: IO ()
main = writeVerilogTop Main.top "top" "Out-Verilog/"
