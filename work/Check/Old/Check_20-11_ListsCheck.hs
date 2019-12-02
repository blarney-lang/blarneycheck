-- First attempt at generating lists (a lot of functions used for this!!)

{-# LANGUAGE GADTs #-}

import Blarney

data Prop where
  Assert :: (Bit 1) -> Prop
  Forall :: (Bits a, KnownNat (SizeOf a)) => (a -> Prop) -> Prop
  ForallList :: (Bits a, KnownNat (SizeOf a)) => Integer -> ([a] -> Prop) -> Prop

concatHelp :: [[Integer]] -> Integer -> [[Integer]]
concatHelp [[]] numToPrepend = [[numToPrepend]]
concatHelp (xs:xss) numToPrepend = (numToPrepend:xs):(concatHelp xss numToPrepend)

genPermutations :: Integer -> Integer -> [[Integer]]
genPermutations maxDepth 0 = [[]]
genPermutations maxDepth length = concatMap (concatHelp (genPermutations maxDepth (length-1))) [0 .. maxDepth]

genListInputs :: Integer -> Integer -> Integer -> [[Integer]]
genListInputs maxLength maxDepth currLength
  | maxLength == currLength = genPermutations maxDepth currLength
  | otherwise               = (genPermutations maxDepth currLength) ++ (genListInputs maxLength maxDepth (currLength+1))

applyToProp :: Integer -> Prop -> [Prop]
applyToProp n (Forall f) = map f (map (\x -> unpack (constant x)) [0 .. n])
applyToProp n (ForallList maxLength f) = map f (map (\x -> map (\y -> unpack (constant y)) x) (genListInputs maxLength n 0))

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

check :: Integer -> Prop -> Module()
check depth prop = do
  let testSeq = (checkTwo (2^3-1) prop)

  --globalTime :: Reg (Bit 32) <- makeReg 0

  always do
    testSeq
    
    --globalTime <== globalTime.val + 1
    --display "Time: " (globalTime.val)


top :: Module ()
top = do
  let propSubComm = Forall \a -> Forall \b -> Assert ((a :: Bit 4)-b.==.b-a)
  check (2^3-1) propSubComm


main :: IO ()
main = writeVerilogTop top "top" "Out-Verilog/"
