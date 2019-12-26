-- Create a series class that allows all the inputs up to a depth to be generated

{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ConstraintKinds #-}

import Blarney
import Check.Generator

data TestBench = TestBench 
    { runTest :: Action ()
    , increment :: Action ()
    , isDone :: Bit 1
    } deriving (Generic, Interface)

data Prop where
  Assert :: (Bit 1) -> Prop
  Forall :: SizedBits a => String -> (a -> Prop) -> Prop


makeAssertTestBench :: (Bit 1) -> Action () -> TestBench
makeAssertTestBench result dispValues = 
  TestBench { runTest = when (inv result) do
                          dispValues >> display "failed the test." >> finish
            , increment = noAction
            , isDone = 1
            }

doActionList :: [Action ()] -> Action ()
doActionList xs = foldr (>>) noAction xs

extractDones :: [TestBench] -> [Bit 1]
extractDones [] = []
extractDones (tb:tbs) = (tb.isDone):(extractDones tbs)


runAllTbs :: [TestBench] -> Action ()
runAllTbs [] = noAction
runAllTbs (tb:tbs) = tb.runTest >> (runAllTbs tbs)


createIncrementAction :: SizedBits a => (TestBench, Reg a) -> Action ()
createIncrementAction (tb, register) = do
    if (bitsGen.isLast $ register.val)
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



firstHot :: KnownNat n => Bit n -> Bit n
firstHot x = x .&. ((inv x) .+. 1);

top :: Module ()
top = do
  --let propSubComm = Forall "A" \a -> Forall "B" \b -> Assert ((a :: Bit 2)-b.==.b-a)
  let prop_OneIsHot = Forall "A" \x -> Assert (countOnes (firstHot (x :: Bit 4)) .==. ((x .==. 0) ? (0, 1)))
  let prop_HotBitCommon = Forall "B" \x -> Assert ((x :: Bit 4) .&. (firstHot x) .==. (firstHot x))
  let prop_HotBitFirst = Forall "C" \x -> Assert ((x :: Bit 4) .&. ((firstHot x) - 1) .==. 0)
  
  t1Complete <- check prop_OneIsHot
  t2Complete <- check prop_HotBitCommon
  t3Complete <- check prop_HotBitFirst
  --subComplete <- check propSubComm

  let complete = t1Complete .&. t2Complete .&. t3Complete
  --let complete = subComplete
  
  always do
    when complete do
      finish


main :: IO ()
main = writeVerilogTop top "top" "Out-Verilog/"
