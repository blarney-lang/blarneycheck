-- Blarney imports
import Blarney
import Blarney.Queue
import Blarney.Stream
import Check.Check

firstHot :: KnownNat n => Bit n -> Bit n
firstHot x = x .&. ((inv x) .+. 1)

countOnes :: (KnownNat n, KnownNat (1 + Log2 n), 1 <= 1 + Log2 n) => Bit n -> Bit (1 + Log2 n)
countOnes b = b.toBitList.(map zeroExtend).sumList

testBench :: Stream (Bit 8) -> Module (Stream (Bit 8))
testBench bytesIn = do  
  bytesOut :: Queue (Bit 8) <- makeQueue
  
  let prop_OneIsHot =     Forall \(x :: Bit 28) -> Assert ((x .==. 0 ? (0, 1)) .==. countOnes (firstHot x))
  let prop_HotBitCommon = Forall \(x :: Bit 28) -> Assert (x .&. (firstHot x) .==. (firstHot x))
  let prop_HotBitFirst =  Forall \(x :: Bit 28) -> Assert (x .&. ((firstHot x) - 1) .==. 0)
  
  let properties = [
          ("OneIsHot", prop_OneIsHot)
        , ("HotBitCommon", prop_HotBitCommon)
        , ("HotBitFirst", prop_HotBitFirst)
        ]

  checkPureFPGA bytesOut properties

  always do
    when (bytesIn.canPeek) do
      bytesIn.consume

  return (bytesOut.toStream)


main :: IO ()
main = do
  writeVerilogModule testBench "Top" "Top-Verilog/"
