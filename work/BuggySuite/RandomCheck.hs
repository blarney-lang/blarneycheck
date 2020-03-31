import Blarney
import Check.Check

newtype MyRandBit n = MyRandBit (RandBit n) deriving (Generic, Bits)
instance Cmp (MyRandBit n) where
  (MyRandBit a) .==. (MyRandBit b) = a .==. b

instance KnownNat n => Generator (MyRandBit n) where
  initial = MyRandBit $ constant 123
  next (MyRandBit bit) = MyRandBit $ next bit
  isFinal current = initial .==. (next current)

testBench :: Module ()
testBench = do
  let prop_DisplayCustom = ("DisplayCustom", Forall \(mrb :: MyRandBit 8) -> WhenAction 1 (display "Mrb: " $ pack mrb))
  let prop_Display = ("Display", Forall \(rb :: RandBit 8) -> WhenAction 1 (display "Rb: " rb))

  _ <- check noAction [prop_DisplayCustom, ("True", Assert 1)] 1
  
  return ()

main :: IO ()
main = writeVerilogTop testBench "top" "Out-Verilog/"