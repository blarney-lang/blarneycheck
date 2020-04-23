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
  range = middleVal (valueOf @n) + 1

instance KnownNat n => Generator (Upper n) where
  initial = Upper $ constant $ middleVal (valueOf @n)
  next (Upper current) = Upper $ current .+. 1
  isFinal (Upper current) = current .==. ones
  range = middleVal (valueOf @n) + 1

testBench :: Module ()
testBench = do
  let prop_Associativity1 = Forall \((Lower x) :: Lower 10) -> Forall \(y :: Bit 10) -> Forall \(z :: Bit 10) -> Assert ((x .+. y) .+. z .==. x .+. (y .+. z))
  let prop_Associativity2 = Forall \((Upper x) :: Upper 10) -> Forall \(y :: Bit 10) -> Forall \(z :: Bit 10) -> Assert ((x .+. y) .+. z .==. x .+. (y .+. z))
  let prop_Commutativity1 = Forall \((Lower x) :: Lower 15) -> Forall \(y :: Bit 15) -> Assert (x .+. y .==. y .+. x)
  let prop_Commutativity2 = Forall \((Upper x) :: Upper 15) -> Forall \(y :: Bit 15) -> Assert (x .+. y .==. y .+. x)
  
  let properties = [
          ("Associativity1", prop_Associativity1)
        , ("Associativity2", prop_Associativity2)
        , ("Commutativity1", prop_Commutativity1)
        , ("Commutativity2", prop_Commutativity2)
        ]

  _ <- checkPure properties
  --estimateTestCaseCount properties 0
  
  return ()

main :: IO ()
main = writeVerilogTop testBench "top" "Out-Verilog/"