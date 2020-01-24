{-# LANGUAGE GADTs #-}

import Blarney
import Blarney.RAM
import Check4.Stack
import Check4.Generator

data TestBench = TestBench 
    { runTest :: Action ()
    , increment :: Action ()
    , isDone :: Bit 1
    , reset :: Action ()
    } deriving (Generic, Interface)

data Prop where
  Assert :: (Bit 1) -> Prop
  Forall :: Generator a => String -> (a -> Prop) -> Prop
  WhenAction :: (Bit 1) -> Action() -> Prop


isAssertProp :: Prop -> Bool
isAssertProp (Assert _) = True
isAssertProp (WhenAction _ _) = False
isAssertProp (Forall _ f) = isAssertProp (f initial)

splitProps :: [Prop] -> ([Prop], [Prop])
splitProps [] = ([], [])
splitProps (prop:props) = if (isAssertProp prop) then (prop:assert, sideEffect) else (assert, prop:sideEffect)
  where (assert, sideEffect) = splitProps props


makeAssertTestBench :: (Bit 1) -> Action() -> TestBench
makeAssertTestBench result dispSetValues = 
  TestBench { runTest = when (inv result) (dispSetValues >> display "Failed the test." >> finish)
            , increment = noAction
            , isDone = constant 1
            , reset = noAction
            }

combineTestBenches :: Module [TestBench] -> Module TestBench
combineTestBenches mtbs = do
  tbs <- mtbs
  return (combineTBs tbs)
  where
    combineTBs [] = error "No Assert Props given"
    combineTBs [tb] = tb
    combineTBs (tb:tbs) =
      TestBench { runTest = (when (inv (tb.isDone)) (tb.runTest)) >> (when (inv (tb2.isDone)) (tb2.runTest))
                , increment = tb.increment >> tb2.increment
                , isDone = tb.isDone .&. tb2.isDone
                , reset = tb.reset >> tb2.reset
                }
        where tb2 = combineTBs tbs


displayVarAndAbove :: SizedBits a => (String, a) -> Action () -> Action ()
displayVarAndAbove (name, genVal) dispAbove = dispAbove >> display_ name "=" (pack genVal) ", "

createIncrementAction :: SizedBits a => (TestBench, Reg a) -> Action ()
createIncrementAction (tb, register) = do
    if (isFinal $ register.val) then do
      if tb.isDone 
        then
          noAction
        else do
          (register <== initial)
          tb.increment
    else do
      register <== (next $ register.val)


{-Create the base TestBench here-}
checkGen :: Action () -> Prop -> Module (TestBench)
checkGen dispValues (Assert p) = do
  return (makeAssertTestBench p dispValues)
checkGen dispValues (Forall name f) = do
  gen <- makeReg initial
  tb <- checkGen (displayVarAndAbove (name, gen.val) dispValues) (f (gen.val))
  return TestBench { runTest = tb.runTest
                   , increment = createIncrementAction (tb, gen)
                   , isDone = isFinal (gen.val) .&. tb.isDone
                   , reset = (gen <== initial) >> tb.reset
  }
checkGen _ (WhenAction _ _) = error "When in Assert check"

runSeProp :: Prop -> Action()
runSeProp (WhenAction guard effect) = when guard effect

applyPropIndexed :: KnownNat n => Integer -> [Prop] -> Bit n -> Action ()
applyPropIndexed _ [] _ = noAction
applyPropIndexed currVal [prop] idx = do
  when (idx .==. (constant currVal)) (runSeProp prop)
applyPropIndexed currVal (prop:props) idx = do
  when (idx .==. (constant currVal)) (runSeProp prop)
  applyPropIndexed (currVal+1) props idx


check :: Action() -> [Prop] -> Integer -> Module(Bit 1)
check rst props depth = do
  let (asserts, sideEffects) = splitProps props
  tb <- combineTestBenches $ mapM (checkGen noAction) asserts
  dfsChecked :: RAM (Bit 8) (Bit 5) <- makeRAM -- TODO: Replace 32 with something like Bit (log depth) Bit log(length sideEffects)
  currDepth :: Reg (Bit 8) <- makeReg 0

  globalTime :: Reg (Bit 32) <- makeReg 0
  runTests :: Reg (Bit 1) <- makeReg 1
  allDone :: Reg (Bit 1) <- makeReg 0
  loadedRam :: Reg (Bit 1) <- makeReg 0
  incrementDfs :: Reg (Bit 1) <- makeReg 0
  always do
    if (runTests.val) then do
      display "Run tests" (currDepth.val)
      tb.runTest
      tb.increment
      runTests <== inv (tb.isDone)
      when (tb.isDone) (tb.reset)
    else do
      display "Inc depth" (currDepth.val)
      if (incrementDfs.val) then do
        if (loadedRam.val) then do
          if ((dfsChecked.out) + 1 .>=. (constant (toInteger (length sideEffects)))) then do
            if (currDepth.val) .==. zero then do
              allDone <== 1
              display $ "All tests passed to depth " ++ (show depth)
              finish
            else do
              store dfsChecked (currDepth.val) 0
              loadedRam <== 0
              currDepth <== (currDepth.val) - 1
          else do
              store dfsChecked (currDepth.val) ((dfsChecked.out) + 1)
              loadedRam <== 0
              currDepth <== 0
              runTests <== 1
              incrementDfs <== 0
        else do
          load dfsChecked (currDepth.val)
          loadedRam <== 1
      else do
        if (currDepth.val .>=. (constant depth)) then do
          incrementDfs <== 1
          when (currDepth.val .!=. zero) (currDepth <== (currDepth.val) - 1)
          rst
          display "Resetting" (currDepth.val)
        else do
          if (loadedRam.val) then do
            applyPropIndexed 0 sideEffects (dfsChecked.out)
            currDepth <== (currDepth.val) + 1
            runTests <== 1
            loadedRam <== 0
          else do
            load dfsChecked (currDepth.val)
            loadedRam <== 1
    globalTime <== globalTime.val + 1
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
  stack1 :: Stack 10 (Bit 4) <- makeStack
  stack2 :: Stack 10 (Bit 4) <- makeStack
  let stackPush = WhenAction (1) (display "Pushing" >> (stack1.push1) 0 >> (stack2.push1) 0) -- (stack1.overflow.inv) .&. (stack2.overflow.inv)
  let stackPop = WhenAction (1) (display "Popping" >> (stack1.pop) 1 >> (stack2.pop) 1) -- (stack1.underflow.inv) .&. (stack2.underflow.inv)
  let propSort = Assert (stack1.top1 .==. stack2.top1)
  done <- check ((stack1.pop) (stack1.size) >> (stack2.pop) (stack2.size)) [propSort, stackPush, stackPop] 2
  --always do
    --when done
      --finish
  return ()


main :: IO ()
main = writeVerilogTop top "top" "Out-Verilog/"
