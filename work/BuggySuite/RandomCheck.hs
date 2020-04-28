{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PolyKinds #-}

import Blarney
import Check.Check

newtype CustomRandBit n = CustomRandBit (Bit n) deriving (Generic, Bits)

instance KnownNat n => Generator (CustomRandBit n) where
  initial = unpack $ constant 0x0
  next (CustomRandBit bit) = CustomRandBit $ pack $ next $ (unpack bit :: RandBits (Bit n) (Seed 196 1))
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
  let prop_DisplayCustom  = Forall \(CustomRandBit crb :: CustomRandBit 8)                  -> WhenAction true (display_ crb ", ")
  let prop_DisplayBuiltin = Forall \(RandBits brb :: RandBits (Bit 4, Bit 4) (Seed 0xc4 1)) -> WhenAction true (display_ brb ", ")

  let properties = [
          ("New Line", WhenAction 1 $ display "Custom Random:")
        , ("Display Custom Random", prop_DisplayCustom)
        , ("New Line", WhenAction 1 $ display "\n\nBuiltin Random:")
        , ("Display Builtin Random", prop_DisplayBuiltin)
        , ("True", Assert 1)
        ]

  _ <- check properties noAction 1
  --estimateTestCaseCount properties 1
  
  return ()

main :: IO ()
main = writeVerilogTop testBench "top" "Out-Verilog/"