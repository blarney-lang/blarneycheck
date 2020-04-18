{-# LANGUAGE UndecidableInstances #-}

import Blarney
import Check.Check

newtype MemAddr = MemAddr (Bit 41) deriving (Generic, Bits)

instance Generator MemAddr where
  initial = unpack (constant $ 2^12-1)
  next current = unpack $ pack current + (constant $ 2^12)
  isFinal current = pack current .==. ones

testBench :: Module ()
testBench = do
  let prop_MRGT10 = ("MemAddr GT10", Forall \(mr :: MemAddr) -> Assert (slice @11 @0 (pack mr) .==. ones))

  _ <- checkPure [prop_MRGT10]
  
  return ()

main :: IO ()
main = writeVerilogTop testBench "top" "Out-Verilog/"
