{-# LANGUAGE GADTs #-}

import Blarney
import Check5.Property
import Check5.Stack


splitBranches :: [(Action(), CycleCommand)] -> ([Action()], [CycleCommand])
splitBranches [] = ([], [])
splitBranches ((initialize, cc):xs) = (initialize:inits, cc:ccs)
  where (inits, ccs) = splitBranches xs

doAllActions :: [Action()] -> Action()
doAllActions [] = noAction
doAllActions [act] = act
doAllActions (act:acts) = act >> (doAllActions acts)

check :: Action() -> [Prop] -> Int -> Module(Bit 1)
check rst props depth = do
  currMaxDepth :: Reg (Bit 16) <- makeReg 1

  let (asserts, sideEffects) = splitProps props
  tb <- combinePureTestBenches $ mapM checkGenPure asserts
  branches <- mapM (makeBranchFromImpure depth) sideEffects
  let (inits, ccs) = splitBranches branches
  (tbInit, impureInc, displayFailImpure, allDone) <- makeImpureTestBench depth (currMaxDepth.val) rst ccs
  let allInit = doAllActions (tbInit:inits)

  globalTime :: Reg (Bit 32) <- makeReg 0
  runTests :: Reg (Bit 1) <- makeReg 1
  displayFail :: Reg (Bit 1) <- makeReg 0
  always do
    if (tb.failed .|. displayFail.val) then do
      displayFail <== 1
      when (runTests.val) do
        tb.runTest
        runTests <== 0
      displayFailImpure
      when allDone finish
    else do
      if (runTests.val) then do
        tb.runTest
        tb.increment
        runTests <== inv (tb.isDone)
        when (tb.isDone) (tb.reset)
      else do
        if allDone then do
          _ <- display "All tests passed to depth " (currMaxDepth.val)
          if (currMaxDepth.val .>=. (constant (toInteger depth))) then do
            finish
          else do
            --display "Increasing depth"
            currMaxDepth <== (currMaxDepth.val) + 1
            allInit
        else do
          runTests <== 1
          impureInc
    globalTime <== globalTime.val + 1
      --display "Time: " (globalTime.val)
      --when (tb.isDone) do
        --display "Test pass"
  return allDone



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
  stack1 :: Stack 10 (Bit 1) <- makeStack
  stack2 :: Stack 10 (Bit 1) <- makeStack
  let stackPush = Forall "X" \x -> (WhenAction "Push" (1) ((stack1.push1) x >> (stack2.push1) x)) -- (stack1.overflow.inv) .&. (stack2.overflow.inv)
  let stackPop = WhenAction "Pop" (1) ((stack1.pop) 1 >> (stack2.pop) 1) -- (stack1.underflow.inv) .&. (stack2.underflow.inv)
  let propSort = Assert "Stack top equal" (stack1.top2 .==. 0)
  done <- check ((stack1.pop) (stack1.size) >> (stack2.pop) (stack2.size)) [propSort, stackPush, stackPop] 3
  --always do
    --when done
      --finish
  return ()


main :: IO ()
main = writeVerilogTop top "top" "Out-Verilog/"
