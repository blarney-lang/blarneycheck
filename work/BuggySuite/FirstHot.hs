import Blarney
import Check.Check

firstHot :: KnownNat n => Bit n -> Bit n
firstHot x = x .&. ((inv x) .+. 1)

countOnes :: KnownNat n => Bit n -> Bit 1
countOnes b = countOnesDown (widthOf b - 1)
  where
    countOnesDown i = if (i >= 0) then (unsafeAt i b) .|. (countOnesDown (i-1)) else 0

testBench :: Module ()
testBench = do
  let prop_OneIsHot =     Forall \(x :: Bit 28) -> Assert (countOnes (firstHot x) .==. (x .==. 0) ? (0, 1))
  let prop_HotBitCommon = Forall \(x :: Bit 28) -> Assert (x .&. (firstHot x) .==. (firstHot x))
  let prop_HotBitFirst =  Forall \(x :: Bit 28) -> Assert (x .&. ((firstHot x) - 1) .==. 0)
  
  let properties = [
          ("OneIsHot", prop_OneIsHot)
        , ("HotBitCommon", prop_HotBitCommon)
        , ("HotBitFirst", prop_HotBitFirst)
        ]

  _ <- checkPure properties
  --estimateTestCaseCount properties 0
  
  return ()

main :: IO ()
main = writeVerilogTop testBench "top" "Out-Verilog/"