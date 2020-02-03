import Blarney
import Check.Check


testBench :: Module ()
testBench = do
  --let propSubComm = Forall "A" \a -> Forall "B" \b -> Assert ((a :: Bit 2)-b.==.b-a)
  let prop_Associativity = Assert "Associativity" \(x :: Bit 4) -> \(y :: Bit 4) -> \(z :: Bit 4) -> ((x .+. y) .+. z .==. x .+. (y .+. z))
  let prop_Commutativity = Assert "Commutativity" \(x :: Bit 4) -> \(y :: Bit 4) -> (x .+. y .==. y .+. x)
  
  _ <- check noAction [prop_Associativity, prop_Commutativity] 0
  
  return ()

main :: IO ()
main = writeVerilogTop testBench "top" "Out-Verilog/"