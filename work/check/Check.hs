-- Greatly shortened list generation with haskell list comprehension
-- Added displaying inputs to tests

{-# LANGUAGE GADTs #-}

import Blarney

data Prop where
  Assert :: (Bit 1) -> Prop
  Forall :: (Bits a, KnownNat (SizeOf a)) => (a -> Prop) -> Prop
  ForallList :: (Bits a, KnownNat (SizeOf a)) => Integer -> ([a] -> Prop) -> Prop

genListInputs :: Integer -> Integer -> [[Integer]]
genListInputs maxDepth currLength
  | currLength <= 0 = [[]]
  | otherwise       = []:[ x:xs | x <- [0 .. maxDepth], xs <- (genListInputs maxDepth (currLength-1))]

applyToProp :: Integer -> (Prop, Action ())  -> [(Prop, Action ())]
applyToProp n (Forall f, disp) = 
  [(f (unpack (constant x)), disp >> display_ x ", ") | x <- [0 .. n]]
applyToProp n (ForallList maxLength f, disp) = 
  [(f (map (\x -> (unpack (constant x))) xs), disp >> display_ (fshowList xs) ", ") | xs <- (genListInputs n maxLength)]

genInputs :: Integer -> [(Prop, Action ())] -> [(Prop, Action ())]
genInputs _ [] = []
genInputs n props@((Assert _, _):xs) = props
genInputs n props@((Forall _, _):xs) = 
  genInputs n (concatMap (applyToProp n) props)
genInputs n props@((ForallList _ _, _):xs) = 
    genInputs n (concatMap (applyToProp n) props)

displayProps :: [(Prop, Action ())] -> Action ()
displayProps [] = finish
displayProps ((Assert p, disp):props) = do
  display_ "Test with inputs: "
  disp
  display "\tresult: " (p)
  displayProps props

checkTwo :: Integer -> Prop -> Action ()
checkTwo depth prop = displayProps (genInputs depth [(prop, noAction)])

check :: Prop -> Module()
check prop = do
  let testSeq = (checkTwo (2^3-1) prop)

  --globalTime :: Reg (Bit 32) <- makeReg 0

  always do
    testSeq
    
    --globalTime <== globalTime.val + 1
    --display "Time: " (globalTime.val)



twoSort :: (Bit 8, Bit 8) -> (Bit 8, Bit 8)
twoSort (a, b) = (b - a) + 5 .>. 5 ? ((a, b), (b, a))
--twoSort (a, b) = a .<. b ? ((a, b), (b, a))

bubble :: [Bit 8] -> [Bit 8]
bubble [] = []
bubble [x] = [x]
bubble (x:y:rest) = bubble (small:rest) ++ [big]
  where (small, big) = twoSort (x, y)

sort :: [Bit 8] -> [Bit 8]
sort [] = []
sort (x:xs) = smallest : sort rest
  where (smallest:rest) = bubble (x:xs)

isSorted :: [Bit 8] -> Bit 1
isSorted [] = 1
isSorted [x] = 1
isSorted (x1:x2:xs) = (x1 .<=. x2) .&. isSorted (x2:xs)



top :: Module ()
top = do
  --let propSubComm = Forall \a -> Forall \b -> Assert ((a :: Bit 4)-b.==.b-a)
  let propSort = Forall \b -> ForallList 2 \a -> Assert (isSorted (sort a) .&. b)
  check propSort


main :: IO ()
main = writeVerilogTop top "top" "Out-Verilog/"
