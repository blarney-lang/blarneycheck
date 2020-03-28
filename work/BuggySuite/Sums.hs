import Blarney
import Check.Check

associativity :: KnownNat n => Bit n -> Bit n -> Bit n -> Bit 1
associativity x y z = (x .+. y) .+. z .==. x .+. (y .+. z)

commutativity :: KnownNat n => Bit n -> Bit n -> Bit 1
commutativity x y = x .+. y .==. y .+. x

testBench :: Module ()
testBench = do
  --let propSubComm = Forall "A" \a -> Forall "B" \b -> Assert ((a :: Bit 2)-b.==.b-a)
  let prop_Associativity = ("Associativity", Forall \(x :: Bit 10) -> Forall \(y :: Bit 10) -> Forall \(z :: Bit 10) -> Assert (associativity x y z))
  let prop_Commutativity = ("Commutativity", Forall \(x :: Bit 15) -> Forall \(y :: Bit 15) -> Assert (commutativity x y))
  
  _ <- check noAction [prop_Associativity, prop_Commutativity] 0
  
  return ()

main :: IO ()
main = writeVerilogTop testBench "top" "Out-Verilog/"