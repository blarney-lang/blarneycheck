import Blarney
import Check.Check

testBench :: Module ()
testBench = do
  --let propSubComm = Forall "A" \a -> Forall "B" \b -> Assert ((a :: Bit 2)-b.==.b-a)
  let prop_Associativity = Forall \(x :: Bit 10) -> Forall \y -> Forall \z -> Assert ((x .+. y) .+. z .==. x .+. (y .+. z))
  let prop_Commutativity = Forall \(x :: Bit 15) -> Forall \y ->              Assert (x .+. y .==. y .+. x)
  
  let properties = [
          ("Associativity", prop_Associativity)
        , ("Commutativity", prop_Commutativity)
        ]

  _ <- checkPure properties
  --estimateTestCaseCount properties 0
  
  return ()

main :: IO ()
main = writeVerilogTop testBench "top" "Out-Verilog/"