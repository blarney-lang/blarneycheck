import Blarney
import Check.Check

firstHot :: KnownNat n => Bit n -> Bit n
firstHot x = x .&. ((inv x) .+. 1)

testBench :: Module ()
testBench = do
  --let propSubComm = Forall "A" \a -> Forall "B" \b -> Assert ((a :: Bit 2)-b.==.b-a)
  let prop_OneIsHot =     ("OneIsHot",     Forall \(x :: Bit 8) -> Assert (countOnes (firstHot x) .==. ((x .==. 0) ? (0, 1))))
  let prop_HotBitCommon = ("HotBitCommon", Forall \(x :: Bit 8) -> Assert (x .&. (firstHot x) .==. (firstHot x)))
  let prop_HotBitFirst =  ("HotBitFirst",  Forall \(x :: Bit 8) -> Assert (x .&. ((firstHot x) - 1) .==. 0))
  
  _ <- check noAction [prop_OneIsHot, prop_HotBitCommon, prop_HotBitFirst] 0
  
  return ()

main :: IO ()
main = writeVerilogTop testBench "top" "Out-Verilog/"