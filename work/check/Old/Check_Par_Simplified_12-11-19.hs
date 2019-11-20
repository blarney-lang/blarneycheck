{-# LANGUAGE GADTs #-}

import Blarney

data Prop where
  Assert :: (Bit 1) -> Prop
  Forall :: (Bits a, KnownNat (SizeOf a)) => (a -> Prop) -> Prop


applyToProp :: Integer -> Prop -> [Prop]
applyToProp n (Forall f) = map f (map (\x -> unpack (constant x)) [0 .. n])

genInputs :: Integer -> [Prop] -> [Prop]
genInputs _ [] = []
genInputs n ((Assert p):xs) = ((Assert p):xs)
genInputs n ((Forall f):xs) = 
  genInputs n (concat (map (applyToProp n) ((Forall f):xs)))

displayProps :: [Prop] -> Action ()
displayProps [] = finish
displayProps ((Assert p):props) = do
  display "Test, result: " (p)
  displayProps props

checkTwo :: Integer -> Prop -> Action ()
checkTwo depth prop = displayProps (genInputs depth [prop])

check :: Integer -> Prop -> Module()
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
