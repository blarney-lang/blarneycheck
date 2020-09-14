{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PolyKinds #-}

import Blarney
import BlarneyCheck

newtype CustomRandBit n = CustomRandBit (Bit n) deriving (Generic, Bits)

instance KnownNat n => Generator (CustomRandBit n) where
  initial = unpack $ constant 0x0
  next (CustomRandBit bit) = CustomRandBit $ pack $ next $ (unpack bit :: RandBits (Bit n) (Seed 196 42))
  isFinal current = pack (initial :: CustomRandBit n) .==. current.next.pack
  range = 2^(valueOf @n)

-- For "RandBits (Bit n) (Seed i s)":
--  Nat 'i' is initial value
--  Nat 's' is seed for ordering values
-- In general there are 2^(n-2)-1 possible orderings thus 0 <= 's' < 2^(n-2)-1
-- For larger 's' >= 2^(n-2)-1 + m, with m >= 0, ordering will be same as if 's' = m
-- Note that, although pseudo-random, the order of the lowest 4 bits is rarely affected by 's' values
-- Also note that when 's' = 2^(n-2)-n, the order is the same as the normal generator: +1
testBench :: Module ()
testBench = do
  genValue <- makeReg (0, 0)

  -- Example of custom random generator and the values it generates
  let prop_DisplayCustom = Forall \(CustomRandBit crb :: CustomRandBit 8)                  -> WhenAction true (display_ crb ", ")
  -- Example property which will fail above 2^63, but could never be found with normal checking
  let prop_UseBuiltin = Forall \(RandBits brb :: RandBits (Bit 32, Bit 32) (Seed 0xc4 42)) -> WhenAction true (genValue <== brb)

  let properties = [
          ("New Line", WhenAction 1 $ display "Custom Random Values:")
        , ("Display Custom Random", prop_DisplayCustom)
        , ("New Line", WhenAction 1 $ display "\n\nBuiltin Random:")
        , ("Builtin_Random", prop_UseBuiltin)
        , ("Fail only at large value", Assert (genValue.val.fst .<. 0x7FffFFff))
        ]

  _ <- check properties noAction 1
  --estimateTestCaseCount properties 1
  
  return ()

-- Code generation
main :: IO ()
main = writeVerilogTop testBench "top" "RandomCheck-Verilog/"
