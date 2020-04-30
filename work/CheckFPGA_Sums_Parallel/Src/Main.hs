-- Blarney imports
import Blarney
import Blarney.Queue
import Blarney.Stream
import Check.Check

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
  next current = unpack $ pack current .+. 1
  isFinal current = pack current .==. ones
  range = middleVal (valueOf @n)


testBench :: Stream (Bit 8) -> Module (Stream (Bit 8))
testBench bytesIn = do  
  bytesOut :: Queue (Bit 8) <- makeQueue
  
  let prop_Associativity1 = Forall \(Lower x) -> Forall \(y :: Bit 10) -> Forall \z -> Assert ((x .+. y) .+. z .==. x .+. (y .+. z))
  let prop_Associativity2 = Forall \(Upper x) -> Forall \(y :: Bit 10) -> Forall \z -> Assert ((x .+. y) .+. z .==. x .+. (y .+. z))
  let prop_Commutativity1 = Forall \(Lower x) -> Forall \(y :: Bit 15) ->              Assert (x .+. y .==. y .+. x)
  let prop_Commutativity2 = Forall \(Upper x) -> Forall \(y :: Bit 15) ->              Assert (x .+. y .==. y .+. x)
  
  let properties = [
          ("Associativity1", prop_Associativity1)
        , ("Associativity2", prop_Associativity2)
        , ("Commutativity1", prop_Commutativity1)
        , ("Commutativity2", prop_Commutativity2)
        ]

  checkPureFPGA bytesOut properties

  always do
    when (bytesIn.canPeek) do
      bytesIn.consume

  return (bytesOut.toStream)


main :: IO ()
main = do
  writeVerilogModule testBench "Top" "Top-Verilog/"
