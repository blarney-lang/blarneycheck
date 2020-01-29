{-# LANGUAGE GADTs #-}

import Blarney
import Check5.Property
import Check5.Stack

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
          --display "-ImpureEdge"
      else do
        pureTB.runTest
        pureTB.increment
        pureTestsDone <== pureTB.isDone
        when (pureTB.isDone) (pureTB.reset)
        when (pureTB.failed) do
          displayingFail <== 1
        --display "+PureCheck"
        
      globalTime <== globalTime.val + 1
      --display "------TICK------"
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



checkCheck :: Module ()
checkCheck = do
  stackSpec :: Stack (Bit 3) <- makeStackSpec 10
  stack :: Stack (Bit 3) <- makeStack 10
  let stackPush = Forall "X" \x -> (WhenAction "Push" (1) ((stackSpec.push) x >> (stack.push) x))
  let stackPop = WhenAction "Pop" (stackSpec.isEmpty.inv .&. stack.isEmpty.inv) ((stackSpec.pop) >> (stack.pop))


  let propStackTopEq = Assert "Stack top equal" (stackSpec.isEmpty .|. (stackSpec.top .==. stack.top)) (display_ "Gold top " (stackSpec.top) ", " >> (display_ "Other top " (stack.top) ", " ))

  let rst = (stackSpec.clear) >> (stack.clear)
  done <- check rst [propStackTopEq, stackPush, stackPop] 3
  
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
main = writeVerilogTop checkCheck "top" "Out-Verilog/"
