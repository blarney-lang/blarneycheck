import Blarney

twoSort :: (Bit 8, Bit 8) -> (Bit 8, Bit 8)
twoSort (a, b) = a .<. b ? ((a, b), (b, a))

{-
top :: Module ()
top = always do
  display "twoSort (1,2) = " (twoSort (1,2))
  display "twoSort (2,1) = " (twoSort (2,1))
  finish
-}

bubble :: [Bit 8] -> [Bit 8]
bubble [] = []
bubble [x] = [x]
bubble (x:y:rest) = bubble (small:rest) ++ [big]
  where (small, big) = twoSort (x, y)

sort :: [Bit 8] -> [Bit 8]
sort [] = []
sort (x:xs) = smallest : sort rest
  where (smallest:rest) = bubble (x:xs)

firstHot :: Bit 8 -> Bit 8
firstHot x = x .&. ((inv x) .+. 1);

top :: Module ()
top = always do
  let input = 8
  display "fh " input " = " (firstHot input)
  finish

main :: IO ()
main = writeVerilogTop top "top" "Out-Verilog/"
