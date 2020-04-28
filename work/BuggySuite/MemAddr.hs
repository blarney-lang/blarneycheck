{-# LANGUAGE UndecidableInstances #-}

import Blarney
import Check.Check

newtype MemAddr = MemAddr (Bit 41) deriving (Generic, Bits)

instance Generator MemAddr where
  initial = unpack (constant $ 2^12-1)
  next current = unpack $ pack current + (constant $ 2^12)
  isFinal current = pack current .==. ones
  range = 2^29

testBench :: Module ()
testBench = do
  let prop_MRGT10 = Forall \(MemAddr ma :: MemAddr) -> Assert (slice @11 @0 ma .==. ones)
  let properties = [("MemAddr GT10", prop_MRGT10)]

  _ <- checkPure properties
  --estimateTestCaseCount properties 0
  
  return ()

main :: IO ()
main = writeVerilogTop testBench "top" "Out-Verilog/"
