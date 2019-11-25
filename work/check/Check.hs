{-# LANGUAGE GADTs #-}

import Blarney

data Prop where
  Assert :: (Bit 1) -> Prop
  Forall :: (Bits a, KnownNat (SizeOf a)) => (a -> Prop) -> Prop
  ForallList :: (Bits a, KnownNat (SizeOf a)) => Integer -> ([a] -> Prop) -> Prop

genListInputs :: (Bits a, KnownNat (SizeOf a)) => Integer -> Integer -> [[a]]
genListInputs maxDepth currLength
  | currLength <= 0 = [[]]
  | otherwise       = []:[ (unpack (constant x)):xs | x <- [0 .. maxDepth], xs <- (genListInputs maxDepth (currLength-1))]

applyToProp :: Integer -> Prop -> [Prop]
applyToProp n (Forall f) = map f (map (\x -> unpack (constant x)) [0 .. n])
applyToProp n (ForallList maxLength f) = map f (genListInputs n maxLength)

genInputs :: Integer -> [Prop] -> [Prop]
genInputs _ [] = []
genInputs n props@((Assert _):xs) = props
genInputs n props@((Forall _):xs) = 
  genInputs n (concat (map (applyToProp n) props))
genInputs n props@((ForallList _ _):xs) = 
    genInputs n (concatMap (applyToProp n) props)

displayProps :: [Prop] -> Action ()
displayProps [] = finish
displayProps ((Assert p):props) = do
  display "Test, result: " (p)
  displayProps props

checkTwo :: Integer -> Prop -> Action ()
checkTwo depth prop = displayProps (genInputs depth [prop])

check :: Prop -> Module()
check prop = do
  let testSeq = (checkTwo (2^3-1) prop)

  --globalTime :: Reg (Bit 32) <- makeReg 0

  always do
    testSeq
    
    --globalTime <== globalTime.val + 1
    --display "Time: " (globalTime.val)



twoSort :: (Bit 8, Bit 8) -> (Bit 8, Bit 8)
twoSort (a, b) = b - a .>. 0 ? ((a, b), (b, a))

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
  let propSort = ForallList 1 \a -> Assert (isSorted (sort a))
  check propSort


main :: IO ()
main = writeVerilogTop top "top" "Out-Verilog/"
