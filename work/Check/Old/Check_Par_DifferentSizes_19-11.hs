-- Using prop a is more restrictive and generally wont work, this is a dead end

{-# LANGUAGE GADTs #-}

import Blarney

data Prop a where
  Assert :: (Bit 1) -> Prop (Bit 1)
  Forall :: (Bits a, KnownNat (SizeOf a), Bits b, KnownNat (SizeOf b)) => (a -> Prop b) -> Prop a


applyToProp :: (Bits a, KnownNat (SizeOf a), Bits b, KnownNat (SizeOf b)) => Integer -> Prop a -> [Prop b]
applyToProp n (Forall (f :: (a -> Prop b))) = map f (map (\x -> unpack (constant x)) [0 .. n])

genInputs :: (Bits a, KnownNat (SizeOf a)) => Integer -> [Prop a] -> [Prop (Bit 1)]
genInputs _ [] = []
genInputs n props@((Assert _):xs) = props
genInputs n props@((Forall _):xs) = 
  genInputs n (concat (map (applyToProp n) props))

displayProps :: (Bits a, KnownNat (SizeOf a)) => [Prop a] -> Action ()
displayProps [] = finish
displayProps ((Assert p):props) = do
  display "Test, result: " (p)
  displayProps props

checkTwo :: (Bits a, KnownNat (SizeOf a)) => Integer -> Prop a -> Action ()
checkTwo depth prop = displayProps (genInputs depth [prop])

check :: (Bits a, KnownNat (SizeOf a)) => Integer -> Prop a -> Module()
check depth prop = do
  let testSeq = (checkTwo (2^3-1) prop)

  --globalTime :: Reg (Bit 32) <- makeReg 0

  always do
    testSeq
    
    --globalTime <== globalTime.val + 1
    --display "Time: " (globalTime.val)


top :: Module ()
top = do
  let propSubComm = Forall \a -> Forall \b -> Assert ((a :: Bit 4)-b.==.b-a)
  check (2^3-1) propSubComm


main :: IO ()
main = writeVerilogTop top "top" "Out-Verilog/"
