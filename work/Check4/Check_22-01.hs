{-# LANGUAGE GADTs #-}

import Blarney
import Blarney.RAM
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
  TestBench { runTest = when (inv result) do dispSetValues >> display "Failed the test." >> finish
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
                , increment = noAction
                , isDone = constant 1
                , reset = noAction
                }
        where tb2 = combineTBs tbs


displayVarAndAbove :: SizedBits a => (String, a) -> Action () -> Action ()
displayVarAndAbove (name, genVal) dispAbove = dispAbove >> display_ name "=" (pack genVal) ", "

createIncrementAction :: SizedBits a => (TestBench, Reg a) -> Action ()
createIncrementAction (tb, register) = do
    if (isFinal $ register.val) then 
        if tb.isDone 
          then
            noAction
          else do
            (register <== initial)
            tb.increment
    else
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
checkGen _ (WhenAction _ _) = error "Checking When in Assert check"


runSeProp :: Prop -> Action()
runSeProp (WhenAction guard effect) = when guard effect

applyPropIndexed :: KnownNat n => Integer -> [Prop] -> Bit n -> Action ()
applyPropIndexed _ [] _ = noAction
applyPropIndexed currVal (prop:props) idx = do
  if(idx .==. (constant currVal)) then
    runSeProp prop
  else
    applyPropIndexed (currVal+1) props idx


check :: Action() -> [Prop] -> Integer -> Module(Bit 1)
check reset props depth = do
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
      runTests <== (tb.isDone)
      tb.increment
      tb.runTest
    else do
      if (incrementDfs.val) then do
        if (loadedRam.val) then do
          if ((dfsChecked.out) + 1 .>=. (constant (toInteger (length sideEffects)))) then do
            if (currDepth.val) .==. zero then do
              allDone <== 1
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
        if (currDepth.val .>. (constant depth)) then do
          incrementDfs <== 1
          currDepth <== (currDepth.val) - 1
          reset
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
  let propSort = Forall "Test" \(a :: Bit 3) -> Assert (a .!=. 9)
  done <- check noAction [propSort] 0
  always do
    when done
      finish


main :: IO ()
main = writeVerilogTop top "top" "Out-Verilog/"
