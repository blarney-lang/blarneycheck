import Blarney
import Check.Check

newtype Lower a = Lower (Bit a) deriving (Generic, Bits)
newtype Upper a = Upper (Bit a) deriving (Generic, Bits)

middleVal :: Int -> Integer
middleVal width = 2^(width - 1)

instance KnownNat n => Generator (Lower n) where
  initial = Lower $ constant $ middleVal (valueOf @n)
  next (Lower current) = Lower $ current .-. 1
  isFinal (Lower current) = current .==. zero

instance KnownNat n => Generator (Upper n) where
  initial = Upper $ constant $ middleVal (valueOf @n)
  next (Upper current) = Upper $ current .+. 1
  isFinal (Upper current) = current .==. ones

associativity :: KnownNat n => Bit n -> Bit n -> Bit n -> Bit 1
associativity x y z = (x .+. y) .+. z .==. x .+. (y .+. z)

commutativity :: KnownNat n => Bit n -> Bit n -> Bit 1
commutativity x y = x .+. y .==. y .+. x

testBench :: Module ()
testBench = do
  let prop_Associativity1 = ("Associativity1", Forall \((Lower x) :: Lower 10) -> Forall \(y :: Bit 10) -> Forall \(z :: Bit 10) -> Assert (associativity x y z))
  let prop_Associativity2 = ("Associativity2", Forall \((Upper x) :: Upper 10) -> Forall \(y :: Bit 10) -> Forall \(z :: Bit 10) -> Assert (associativity x y z))
  let prop_Commutativity1 = ("Commutativity1", Forall \((Lower x) :: Lower 15) -> Forall \(y :: Bit 15) -> Assert (commutativity x y))
  let prop_Commutativity2 = ("Commutativity2", Forall \((Upper x) :: Upper 15) -> Forall \(y :: Bit 15) -> Assert (commutativity x y))
  
  _ <- check noAction [prop_Associativity1, prop_Associativity2, prop_Commutativity1, prop_Commutativity2] 0
  
  return ()

main :: IO ()
main = writeVerilogTop testBench "top" "Out-Verilog/"