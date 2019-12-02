{-# LANGUAGE GADTs #-}

import Blarney

data Prop n where
  Assert :: (Bit 1) -> Prop n
  Forall :: KnownNat n => (Bit n -> Prop n) -> Prop n

calculateReqRegs :: Prop n -> Int
calculateReqRegs (Assert _) = 0
calculateReqRegs (Forall f) = 1 + calculateReqRegs (f (constant 0))

displayProp :: KnownNat n => Prop n -> [Reg (Bit n)] -> Action ()
displayProp (Assert p) [] = do
  display "test result: " (p)
displayProp prop@(Assert _) (x:xs) = do
  display_ (x.val) " "
  displayProp prop xs

checkSeq :: KnownNat n => Prop n -> [Reg (Bit n)] -> [Reg (Bit n)] -> Action ()
checkSeq prop@(Assert _) _ usedRegs = displayProp prop usedRegs
checkSeq (Forall f) (reg:regs) usedRegs = checkSeq (f (reg.val)) regs (reg:usedRegs)
checkSeq (Forall f) [] usedRegs = checkSeq (f (constant 0)) [] usedRegs
       
isFull :: (KnownNat n) => Reg (Bit n) -> Bit 1
isFull x = (x.val + 1) .==. 0

incrementRegs :: (KnownNat n) => [Reg (Bit n)] -> Action ()
incrementRegs [] = finish
incrementRegs (x:xs) = do
  if (isFull x)
    then do
      x <== 0
      incrementRegs xs
    else
      x <== x.val + 1

check :: (KnownNat n) => Prop n -> Module()
check prop = do
  regs :: [Reg (Bit n)] <- mapM makeReg (replicate (calculateReqRegs prop) 0)
  let testSeq = (checkSeq prop regs [])

  globalTime :: Reg (Bit 32) <- makeReg 0
  always do
    --(when (globalTime.val .==. 2000) finish)
    globalTime <== globalTime.val + 1
    incrementRegs regs
    testSeq
    display "Time: " (globalTime.val)


top :: Module ()
top = do
  let propSubComm = Forall \a -> Forall \b -> Assert (a-b.==.a) :: Prop 3
  -- Displays results in reverse: ie. increment b to max, then a + 1 and b to max again etc.
  check propSubComm


main :: IO ()
main = writeVerilogTop top "top" "Out-Verilog/"
