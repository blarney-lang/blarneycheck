{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ConstraintKinds #-}

import Blarney
import Blarney.Queue
import Blarney.Stream
import Generator
import Data.Char (ord)

data TestBench = TestBench 
    { runTest :: Action ()
    , increment :: Action ()
    , isDone :: Bit 1
    } deriving (Generic)

data Prop where
  Assert :: (Bit 1) -> Prop
  Forall :: SizedBits a => String -> (a -> Prop) -> Prop

charToByte :: Char -> Bit 8
charToByte ch = constant (toInteger (ord ch))
enqString :: Queue (Bit 8) -> String -> Action()
enqString out str = doActionList $ map (\c -> when (out.notFull) do (enq out (charToByte c))) str


makeAssertTestBench :: (Bit 1) -> Queue (Bit 8) -> TestBench
makeAssertTestBench result out = 
  TestBench { runTest = when (inv result .&. out.notFull) (enq out (charToByte 'F'))
            , increment = noAction
            , isDone = 1
            }

doActionList :: [Action ()] -> Action ()
doActionList xs = foldr (>>) noAction xs

extractDones :: [TestBench] -> [Bit 1]
extractDones [] = []
extractDones (tb:tbs) = (tb.isDone):(extractDones tbs)


runAllTbs :: [TestBench] -> Action()
runAllTbs [] = noAction
runAllTbs (tb:tbs) = tb.runTest >> (runAllTbs tbs)


createIncrementAction :: SizedBits a => (TestBench, Reg a) -> Action ()
createIncrementAction (tb, register) = do
    if (isLast $ register.val)
      then 
        if tb.isDone 
          then
            noAction
          else do
            (register <== initial)
            tb.increment
      else
        register <== (next $ register.val)


displayVarAndAbove :: SizedBits a => (String, Reg a) -> Action() -> Queue (Bit 8) -> Action()
displayVarAndAbove (name, register) dispAbove out = do
  dispAbove
  --out name
  --out "="
  --out (pack (register.val))
  --out ", "


{-Create the base TestBench here-}
checkGen :: Queue (Bit 8) -> (Prop, Action()) -> Module (TestBench)
checkGen out ((Assert p), dispValues) = do
  return (makeAssertTestBench p out)
checkGen out ((Forall name f), dispValues) = do
  inputs <- mapM makeReg [initial]
  tbs <- mapM (checkGen out) (map (\r -> (f (r.val), displayVarAndAbove (name, r) dispValues out)) inputs)
  let combined = zip tbs inputs
  let isDoneComputed = andList $ (map (\myin -> isLast $ myin.val) inputs)++(extractDones tbs)

  return TestBench { runTest = runAllTbs tbs
                   , increment = if isDoneComputed then noAction else (doActionList (map createIncrementAction combined))
                   , isDone = isDoneComputed
  }


check :: Prop -> Queue (Bit 8) -> Reg (Bit 1)-> Module(Bit 1)
check prop out run = do
  tb <- (checkGen out (prop, noAction))

  globalTime :: Reg (Bit 32) <- makeReg 0
  testComplete :: Reg (Bit 1) <- makeReg 0

  always do
    globalTime <== globalTime.val + 1
    when (inv (testComplete.val) .&. run.val) do
      testComplete <== tb.isDone
      tb.increment
      tb.runTest
      --out "R"
      --display "Time: " (globalTime.val)
      when (tb.isDone .&. out.notFull) do
        --out "Test pass"
        --out "Pass"
        enqString out "Pass"
  return (testComplete.val)



firstHot :: KnownNat n => Bit n -> Bit n
firstHot x = x .&. ((inv x) .+. 1);


makeEcho :: Stream (Bit 8) -> Module (Stream (Bit 8))
makeEcho bytesIn = do
  bytesOut :: Queue (Bit 8) <- makeQueue
  runBench :: Reg (Bit 1) <- makeReg 0

  --let prop_OneIsHot = Forall "A" \x -> Assert (countOnes (firstHot (x :: Bit 4)) .==. ((x .==. 0) ? (0, 1)))
  let prop_HotBitCommon = Forall "B" \x -> Assert ((x :: Bit 4) .&. (firstHot x) .==. (firstHot x))
  --let prop_HotBitFirst = Forall "C" \x -> Assert ((x :: Bit 4) .&. ((firstHot x) - 1) .==. 0)
  
  --t1Complete <- check prop_OneIsHot out
  t2Complete <- check prop_HotBitCommon bytesOut runBench
  --t3Complete <- check prop_HotBitFirst out

  always do
    when (bytesIn.canPeek) do
      runBench <== constant 1
      bytesIn.consume

  return (bytesOut.toStream)

main :: IO ()
main = do
  writeVerilogModule makeEcho "Top" "Top-Verilog/"
