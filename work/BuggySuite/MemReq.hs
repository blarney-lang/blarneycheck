{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ConstraintKinds #-}

import Blarney
import Check.Check

newtype MemReq a = SizedBits a deriving (Generic, Bits)

instance (SizedBits a) => Generator (MemReq a) where
  initial = unpack (constant $ 2^12-1)
  next current = unpack $ pack current .+. 2^12
  isFinal current = pack current .==. ones

testBench :: Module ()
testBench = do
  let prop_MRGT10 = ("MemReq GT10", Forall \(mr :: MemReq (Bit 32)) -> Assert (slice @11 @0 (pack mr) .==. ones))

  _ <- check noAction [prop_MRGT10] 0
  
  return ()

main :: IO ()
main = writeVerilogTop testBench "top" "Out-Verilog/"