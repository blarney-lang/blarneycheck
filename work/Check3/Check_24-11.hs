-- Writing some nicer Haskell with list comprehension to generate list inputs.
-- But we still have a fixed sized bitvector input, want to generalise to Bits class - am struggling with Haskell types, want to avoid using extensions
-- Also want to separate input 'generation' from testing (also allows user to specify custom generators)
-- Possibly sequential testing to allow for synthesis.
-- Need for always passing around display action for the result is not nice.
-- successfully test a sorter network

import Blarney
import Check3.Generator

data TestBench = TestBench 
    { runTest :: Action ()
    , increment :: Action (Bit 1)
    } deriving (Generic, Interface)

data Prop where
  Assert :: (Bit 1) -> Prop
  Forall :: Generator a => (a -> Prop) -> Prop


makeAssertTestBench :: (Bit 1) -> TestBench
makeAssertTestBench result = 
  TestBench { runTest = when (inv result) do
                          display "Failed the test." >> finish
            , increment = return (constant 1)
            }

createIncrementAction :: Generator a => (TestBench, a, [Int]) -> Action (Bit 1)
createIncrementAction (tb, state) =
    case (next state) of
      ((NApply done inc):xs) -> do 
                                  if (done) then (reset state >> return tb.increment)
                                  else (inc >> return (constant 0))
      []        -> return tb.increment
    where createIncrementAction2 (tb, []) = return (tb.increment)
          createIncrementAction2 (tb, ((NApply done inc):xs)) = if(done) then (reset state >> createIncrementAction2 (tb, xs)
                                                                else 

    if (nexts)
      then 
        if tb.isDone 
          then
            noAction
          else do
            (register <== bitsGen.initial)
            tb.increment
      else
        register <== (bitsGen.next $ register.val)


displayVarAndAbove :: SizedBits a => (String, Reg a) -> Action () -> Action ()
displayVarAndAbove (name, register) dispAbove = do
  dispAbove
  display_ name "=" (pack (register.val)) ", "


{-Create the base TestBench here-}
checkGen :: (Prop, Action ()) -> Module (TestBench)
checkGen ((Assert p), dispValues) = do
  return (makeAssertTestBench p dispValues)
checkGen ((Forall name f), dispValues) = do
  inputs <- mapM makeReg [bitsGen.initial]
  tbs <- mapM checkGen (map (\r -> (f (r.val), displayVarAndAbove (name, r) dispValues)) inputs)
  let combined = zip tbs inputs
  let isDoneComputed = andList $ (map (\myin -> bitsGen.isLast $ myin.val) inputs)++(extractDones tbs)

  return TestBench { runTest = runAllTbs tbs
                   , increment = if isDoneComputed then noAction else (doActionList (map createIncrementAction combined))
                   , isDone = isDoneComputed
  }


check :: Prop -> Module(Bit 1)
check prop = do
  tb <- (checkGen (prop, noAction))

  globalTime :: Reg (Bit 32) <- makeReg 0
  testComplete :: Reg (Bit 1) <- makeReg 0
  always do
    globalTime <== globalTime.val + 1
    when (inv (testComplete.val)) do
      testComplete <== tb.isDone
      tb.increment
      tb.runTest
      --display "Time: " (globalTime.val)
      when (tb.isDone) do
        display "Test pass"
  return (testComplete.val)



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
  let propSort = ForallList 4 \a -> Assert (isSorted (sort a))
  check propSort


main :: IO ()
main = writeVerilogTop top "top" "Out-Verilog/"
