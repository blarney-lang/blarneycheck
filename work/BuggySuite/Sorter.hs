import Blarney
import Check.Check

twoSort :: KnownNat n => (Bit n, Bit n) -> (Bit n, Bit n)
--twoSort (a :: Bit n, b) = let halfSize = constant (toInteger (valueOf @(n))) in
--                          (b - a) + halfSize .>=. halfSize ? ((a, b), (b, a))
twoSort (a, b) = a .<. b ? ((a, b), (b, a))

bubble :: KnownNat n => [Bit n] -> [Bit n]
bubble [] = []
bubble [x] = [x]
bubble (x:y:rest) = bubble (small:rest) ++ [big]
  where (small, big) = twoSort (x, y)

sort :: KnownNat n => [Bit n] -> [Bit n]
sort [] = []
sort (x:xs) = smallest : sort rest
  where (smallest:rest) = bubble (x:xs)

isSorted :: KnownNat n => [Bit n] -> Bit 1
isSorted [] = 1
isSorted [_] = 1
isSorted (x1:x2:xs) = (x1 .<=. x2) .&. isSorted (x2:xs)

testBench :: Module ()
testBench = do
  let prop_Sorted = Assert "Sorted" (5 :: Int, \(xs :: [Bit 3]) -> (isSorted $ sort xs))
  
  _ <- check noAction [prop_Sorted] 0
  
  return ()

main :: IO ()
main = writeVerilogTop testBench "top" "Out-Verilog/"