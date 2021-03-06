import Blarney
import BlarneyCheck

-- Function under test
firstHot :: KnownNat n => Bit n -> Bit n
firstHot x = x .&. ((inv x) .+. 1)

-- Helper function
countOnes :: (KnownNat n, KnownNat (1 + Log2 n), 1 <= 1 + Log2 n) => Bit n -> Bit (1 + Log2 n)
countOnes b = b.toBitList.(map zeroExtend).sumList

testBench :: Module ()
testBench = do
  let prop_OneIsHot =     Forall \(x :: Bit 25) -> Assert ((x .==. 0 ? (0, 1)) .==. countOnes (firstHot x))
  let prop_HotBitCommon = Forall \(x :: Bit 25) -> Assert (x .&. (firstHot x) .==. (firstHot x))
  let prop_HotBitFirst =  Forall \(x :: Bit 25) -> Assert (x .&. ((firstHot x) - 1) .==. 0)
  
  let properties = [
          ("OneIsHot", prop_OneIsHot)
        , ("HotBitCommon", prop_HotBitCommon)
        , ("HotBitFirst", prop_HotBitFirst)
        ]

  _ <- checkPure properties
  --estimateTestCaseCount properties 0
  
  return ()

-- Code generation
main :: IO ()
main = writeVerilogTop testBench "top" "FirstHot-Verilog/"
