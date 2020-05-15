import Blarney
import Blarney.Core.Utils
import Data.Maybe
import BlarneyCheck

twoSort :: KnownNat n => (Bit n, Bit n) -> (Bit n, Bit n)
twoSort (a, b) = a .<. b ? ((a, b), (b, a))

-- Bubble sorting network
-- ==============

bubble :: KnownNat n => [Bit n] -> [Bit n]
bubble [] = []
bubble [x] = [x]
bubble (x:y:rest) = bubble (small:rest) ++ [big]
  where (small, big) = twoSort (x, y)

sort1 :: KnownNat n => [Bit n] -> [Bit n]
sort1 [] = []
sort1 (x:xs) = smallest : sort1 rest
  where (smallest:rest) = bubble (x:xs)

-- Even odd sorting network
-- ==============

sort2 :: KnownNat n => [Bit n] -> [Bit n]
sort2 [] = []
sort2 [a] = [a]
sort2 list = network!!(length list)
  where network = list : map oddeven networkOdd
        networkOdd = list : map (\(x:xs) -> x:oddeven xs) network
        oddeven [] = []
        oddeven [x] = [x]
        oddeven (x:y:xs) = let (s, b) = twoSort (x, y)
                            in s:b:oddeven xs

-- Both of the above should be faster on FPGA:
--   Simulation must simulate O(n^2) comparators
--   On hardware both only have depth O(n)
-- However the bitonic sorter below is depth O(log^2(n))
-- Therefore FPGA execution should be even faster

-- Util
-- ==============

type MBit n = Maybe (Bit n)

twoSort' :: KnownNat n => (MBit n, MBit n) -> (MBit n, MBit n)
twoSort' pair@(Nothing, _) = pair
twoSort' pair@(_, Nothing) = pair
twoSort' (Just a, Just b) = let (s,l) = a .<. b ? ((a, b), (b, a))
                            in (Just s, Just l)

cmpLists :: KnownNat n => ([MBit n], [MBit n]) -> ([MBit n], [MBit n])
cmpLists ([], []) = ([], [])
cmpLists (x:xs, y:ys) =
  let (s, b) = twoSort' (x, y)
      (sl, bl) = cmpLists (xs, ys)
  in (s:sl, b:bl)
cmpLists _ = undefined

halve :: [a] -> ([a], [a])
halve list = splitAt (length list `div` 2) list

bitonic :: KnownNat n => [MBit n] -> [MBit n]
bitonic [] = []
bitonic [x] = [x]
bitonic list = 
  let (top, bot) = list.halve.cmpLists
  in bitonic top ++ bitonic bot

sort3' :: KnownNat n => [MBit n] -> [MBit n]
sort3' [] = []
sort3' [x] = [x]
sort3' list =
  let (top1, bot1) = list.halve
      (top2, bot2) = cmpLists (top1.sort3'.reverse, bot1.sort3')
  in top2.reverse.bitonic ++ bot2.bitonic

-- Bitonic sorter
-- ==============

sort3 :: KnownNat n => [Bit n] -> [Bit n]
sort3 [] = []
sort3 list = 
  let po2List = expand (2^list.length.log2ceil - list.length)
  in po2List.sort3'.catMaybes
  where expand 0 = map Just list
        expand n = Nothing : expand (n-1)

-- Helper functions
-- ==============

isSorted :: KnownNat n => [Bit n] -> Bit 1
isSorted [] = true
isSorted [_] = true
isSorted (x1:x2:xs) = (x1 .<=. x2) .&. isSorted (x2:xs)

{-
-- Uncomment for second property
allDifferent :: KnownNat n => [Bit n] -> Bit 1
allDifferent [] = 1
allDifferent (x:xs) = allDifferent xs .&. different xs
  where different [] = 1
        different (y:ys) = (x .!=. y) .&. different ys
-}

testBench :: Module ()
testBench = do
  let prop_Sorted = ForallList 27 \(xs :: [Bit 1]) -> Assert (xs.sort3.isSorted)

  -- Useful for forcing error to be found for list with unique values:
--let prop_Sorted = ForallList 8 \(xs :: [Bit 3]) -> Assert' (xs.allDifferent.inv .|. xs.sort2.isSorted) (display_ $ xs.sort2)

  let properties = [("Sorted", prop_Sorted)]

  _ <- checkPure properties
--estimateTestCaseCount properties 0
  
  return ()

-- Code generation
main :: IO ()
main = writeVerilogTop testBench "top" "Out-Verilog/"