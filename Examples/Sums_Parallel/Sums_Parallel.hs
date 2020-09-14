import Blarney
import BlarneyCheck

-- This could be done to a higher extent with a function to generate all of the properties
-- Using one custom data type which allows specifying start and end points (similar to RandBits)

-- Data types for parallel checking
-- ==============

newtype Lower a = Lower (Bit a) deriving (Generic, Bits)
newtype Upper a = Upper (Bit a) deriving (Generic, Bits)

middleVal :: Int -> Integer
middleVal width = 2^(width - 1)

instance KnownNat n => Generator (Lower n) where
  initial = unpack $ constant $ middleVal (valueOf @n) - 1
  next current = unpack $ pack current .-. 1
  isFinal current = pack current .==. zero
  range = middleVal (valueOf @n)

instance KnownNat n => Generator (Upper n) where
  initial = unpack $ constant $ middleVal (valueOf @n)
  next current = unpack $ pack current + 1
  isFinal current = pack current .==. ones
  range = middleVal (valueOf @n)

-- Check function itself
-- ==============

testBench :: Module ()
testBench = do
  let prop_Associativity1 = Forall \(Lower x) -> Forall \(y :: Bit  8) -> Forall \z -> Assert ((x + y) + z .==. x + (y + z))
  let prop_Associativity2 = Forall \(Upper x) -> Forall \(y :: Bit  8) -> Forall \z -> Assert ((x + y) + z .==. x + (y + z))
  let prop_Commutativity1 = Forall \(Lower x) -> Forall \(y :: Bit 12) ->              Assert (x + y .==. y + x)
  let prop_Commutativity2 = Forall \(Upper x) -> Forall \(y :: Bit 12) ->              Assert (x + y .==. y + x)
  
  let properties = [
          ("Associativity1", prop_Associativity1)
        , ("Associativity2", prop_Associativity2)
        , ("Commutativity1", prop_Commutativity1)
        , ("Commutativity2", prop_Commutativity2)
        ]

  _ <- checkPure properties
  --estimateTestCaseCount properties 0
  
  return ()

-- Code generation
main :: IO ()
main = writeVerilogTop testBench "top" "Sums_Parallel-Verilog/"
