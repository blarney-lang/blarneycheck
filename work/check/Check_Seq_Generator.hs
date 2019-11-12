-- Generator allows for creating registers when walking Prop "list" and
-- then just returning interfaces to (display, inc, reset and isfull)

{-# LANGUAGE GADTs #-}

import Blarney

data Generator where
  Empty :: Action () -> Generator
  --      Display result
  Gen :: Generator -> Action () -> Action () -> (Bit 1) -> Action () -> Generator
  --     Gen below     Display     Increment    Is full     Reset

data Prop where
  Assert :: (Bit 1) -> Prop
  Forall :: (Bits a, KnownNat (SizeOf a)) => (a -> Prop) -> Prop

displayProp :: Generator -> Action ()
displayProp (Empty disp) = disp
displayProp (Gen _ disp _ _ _) = disp

displayPropWithValue :: (Bits a, KnownNat (SizeOf a)) => a -> Generator -> Action ()
displayPropWithValue x g = do
  display_ (pack x) " "
  displayProp g

isFinished :: Generator -> Bit 1
isFinished (Empty disp) = 1
isFinished (Gen g _ _ full _) = full .&. (isFinished g)

checkSeq :: Prop -> Module (Generator)
checkSeq (Assert p) = do
  return (Empty (display "Test result: " p))
checkSeq (Forall f) = do
  input <- makeReg (unpack 0)
  g <- checkSeq (f (input.val))
  let displayValue = (displayPropWithValue (input.val) g)
  let increment = (input <== (incReg (input.val)))
  let full = (input.val === ones)
  let reset = (input <== (unpack 0))
  return (Gen g displayValue increment full reset)

incReg :: (Bits a, KnownNat (SizeOf a)) => a -> a
incReg x = unpack ((pack x) + 1)

incrementGen :: Generator -> Action ()
incrementGen (Empty _) = noAction
incrementGen (Gen g _ increment full reset) = do
  if full
    then do
      reset
      incrementGen g
    else
      increment

check :: Prop -> Module(Bit 1)
check prop = do
  g <- (checkSeq prop)

  globalTime :: Reg (Bit 32) <- makeReg 0
  testComplete :: Reg (Bit 1) <- makeReg 0
  always do
    --(when (globalTime.val .==. 2000) finish)
    globalTime <== globalTime.val + 1
    when (inv (testComplete.val)) do
      testComplete <== isFinished g
      incrementGen g
      displayProp g
    --display "Time: " (globalTime.val)
  return (testComplete.val)

firstHot :: KnownNat n => Bit n -> Bit n
firstHot x = x .&. ((inv x) .+. 1);

top :: Module ()
top = do
  --let propSubComm = Forall \a -> Forall \b -> Assert ((a :: Bit 3)-b.==.b-a)
  let prop_OneIsHot = Forall \x -> Assert (countOnes (firstHot (x :: Bit 4)) .==. ((x .==. 0) ? (0, 1)))
  let prop_HotBitCommon = Forall \x -> Assert ((x :: Bit 4) .&. (firstHot x) .==. (firstHot x))
  let prop_HotBitFirst = Forall \x -> Assert ((x :: Bit 4) .&. ((firstHot x) - 1) .==. 0)
  
  t1Complete <- check prop_OneIsHot
  t2Complete <- check prop_HotBitCommon
  t3Complete <- check prop_HotBitFirst

  always do
    when (t1Complete .&. t2Complete .&. t3Complete) do
      finish


main :: IO ()
main = writeVerilogTop top "top" "Out-Verilog/"
