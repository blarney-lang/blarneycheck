import Blarney
import BlarneyCheck

newtype MemAddr = MemAddr (Bit 32) deriving (Generic, Bits)

instance Generator MemAddr where
  -- Initial value is 0x0...0FFF
  initial = unpack (constant $ 2^12-1)
  -- Count up ignoring lowest 12 bits
  next current = unpack $ pack current + (constant $ 2^12)
  -- Done when all bits are ones
  isFinal current = pack current .==. ones
  -- Number of possible values = 2^(41-12)
  range = 2^20

testBench :: Module ()
testBench = do
  let prop_MRGT10 = Forall \(MemAddr ma :: MemAddr) -> Assert (slice @11 @0 ma .==. ones)
  let properties = [("MemAddr GT10", prop_MRGT10)]

  _ <- checkPure properties
  --estimateTestCaseCount properties 0
  
  return ()

-- Code generation
main :: IO ()
main = writeVerilogTop testBench "top" "MemAddr-Verilog/"
