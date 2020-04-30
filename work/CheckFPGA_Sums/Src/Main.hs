-- Blarney imports
import Blarney
import Blarney.Queue
import Blarney.Stream
import Check.Check

testBench :: Stream (Bit 8) -> Module (Stream (Bit 8))
testBench bytesIn = do  
  bytesOut :: Queue (Bit 8) <- makeQueue
  
  let prop_Associativity = Forall \(x :: Bit 10) -> Forall \y -> Forall \z -> Assert ((x .+. y) .+. z .==. x .+. (y .+. z))
  let prop_Commutativity = Forall \(x :: Bit 15) -> Forall \y ->              Assert (x .+. y .==. y .+. x)
  
  let properties = [
          ("Associativity", prop_Associativity)
        , ("Commutativity", prop_Commutativity)
        ]

  checkPureFPGA bytesOut properties

  always do
    when (bytesIn.canPeek) do
      bytesIn.consume

  return (bytesOut.toStream)


main :: IO ()
main = do
  writeVerilogModule testBench "Top" "Top-Verilog/"
