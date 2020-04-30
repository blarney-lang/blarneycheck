-- Blarney imports
import Blarney
import Blarney.Queue
import Blarney.Stream
import Check.Check

twoSort :: KnownNat n => (Bit n, Bit n) -> (Bit n, Bit n)
twoSort (a, b) = a .<. b ? ((a, b), (b, a))


bubble :: KnownNat n => [Bit n] -> [Bit n]
bubble [] = []
bubble [x] = [x]
bubble (x:y:rest) = bubble (small:rest) ++ [big]
  where (small, big) = twoSort (x, y)

sort1 :: KnownNat n => [Bit n] -> [Bit n]
sort1 [] = []
sort1 (x:xs) = smallest : sort1 rest
  where (smallest:rest) = bubble (x:xs)


halve :: [a] -> ([a], [a])
halve [] = ([], [])
halve [x] = ([x], [])
halve (x:y:list) = let (xs, ys) = halve list in (x:xs, y:ys)

-- Try sorting with only bitonic (merge) step
sort2 :: KnownNat n => [Bit n] -> [Bit n]
sort2 [] = []
sort2 [a] = [a]
sort2 list = sort2 l1 ++ sort2 l2
  where (l1, l2) = bitonic (halve list)
        bitonic ([], []) = ([], [])
        bitonic (xs, []) = (xs, [])
        bitonic ([], ys) = ([], ys)
        bitonic ([x], [y]) = let (s, b) = twoSort (x, y) in ([s], [b])
        bitonic (x:xs, y:ys) =
          let (s, b) = twoSort (x, y)
              (sl, bl) = bitonic (xs, ys)
          in (s:sl, b:bl)


isSorted :: KnownNat n => [Bit n] -> Bit 1
isSorted [] = 1
isSorted [_] = 1
isSorted (x1:x2:xs) = (x1 .<=. x2) .&. isSorted (x2:xs)

testBench :: Stream (Bit 8) -> Module (Stream (Bit 8))
testBench bytesIn = do  
  bytesOut :: Queue (Bit 8) <- makeQueue

  let prop_Sorted = ForallList 25 \(xs :: [Bit 1]) -> Assert (isSorted $ sort1 xs)

  let properties = [("Sorted", prop_Sorted)]

  checkPureFPGA bytesOut properties

  always do
    when (bytesIn.canPeek) do
      bytesIn.consume

  return (bytesOut.toStream)


main :: IO ()
main = do
  writeVerilogModule testBench "Top" "Top-Verilog/"
