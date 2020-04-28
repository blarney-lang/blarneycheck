import Blarney
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

testBench :: Module ()
testBench = do
  let prop_Sorted = ForallList 25 \(xs :: [Bit 1]) -> Assert (isSorted $ sort2 xs)
  --let prop_Sorted = ForallList 6 \(xs :: [Bit 1]) -> Assert ((andList xs).inv)

  let properties = [("Sorted", prop_Sorted)]

  _ <- checkPure properties
  --estimateTestCaseCount properties 0
  
  return ()

main :: IO ()
main = writeVerilogTop testBench "top" "Out-Verilog/"