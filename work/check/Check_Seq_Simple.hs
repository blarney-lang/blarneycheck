{-# LANGUAGE GADTs #-}

import Blarney

data Generator where
  Empty :: Action () -> Generator
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

checkSeq :: Prop -> Module (Generator)
checkSeq (Assert p) = do
  return (Empty (display "Test result: " p))
checkSeq (Forall f) = do
  input <- makeReg (unpack 0)
  g <- checkSeq (f (input.val))
  return (Gen g (displayPropWithValue (input.val) g) (input <== unpack (input.getRegVal + 1)) (isFull input) (input <== (unpack 0)))
{-}
checkSeq :: Prop -> Module ()
checkSeq prop@(Assert _) = runOnce (Action (displayProp prop))
checkSeq (Forall f) = do
  input <- makeReg 0
  let m = checkSeq (f (input.val))
  m
-}
getRegVal :: (Bits a, KnownNat (SizeOf a)) => Reg a -> Bit (SizeOf a)
getRegVal x = pack (x.val)

isFull :: (Bits a, KnownNat (SizeOf a)) => Reg a -> Bit 1
isFull x = (x.getRegVal + 1) .==. 0

incrementGen :: Generator -> Action ()
incrementGen (Empty _) = finish
incrementGen (Gen g _ increment full reset) = do
  if full
    then do
      reset
      incrementGen g
    else
      increment

check :: Prop -> Module()
check prop = do
  g <- (checkSeq prop)

  globalTime :: Reg (Bit 32) <- makeReg 0
  always do
    --(when (globalTime.val .==. 2000) finish)
    globalTime <== globalTime.val + 1
    incrementGen g
    displayProp g
    --display "Time: " (globalTime.val)


top :: Module ()
top = do
  let propSubComm = Forall \a -> Forall \b -> Assert ((a :: Bit 3)-b.==.b-a)
  -- Displays results in reverse: ie. increment b to max, then a + 1 and b to max again etc.
  check propSubComm


main :: IO ()
main = writeVerilogTop top "top" "Out-Verilog/"
