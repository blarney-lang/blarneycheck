import Blarney
import Blarney.Queue
import Blarney.Stream
import Check

testBench :: Stream (Bit 8) -> Module (Stream (Bit 8))
testBench bytesIn = do  
  bytesOut :: Queue (Bit 8) <- makeQueue
  
  let prop_Associativity = ("Associativity", Forall \(x :: Bit 10) -> Forall \(y :: Bit 10) -> Forall \(z :: Bit 10) -> Assert ((x .+. y) .+. z .==. x .+. (y .+. z)))
  let prop_Commutativity = ("Commutativity", Forall \(x :: Bit 15) -> Forall \(y :: Bit 15) -> Assert (x .+. y .==. y .+. x))
 
  _ <- check noAction [prop_Associativity, prop_Commutativity] 0 bytesOut

  always do
    when (bytesIn.canPeek) do
      bytesIn.consume

  return (bytesOut.toStream)


main :: IO ()
main = do
  writeVerilogModule testBench "Top" "Top-Verilog/"
