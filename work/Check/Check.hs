-- Can now bit width of input from type so that we can generate all test cases exhaustively

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

applyToProp :: (Prop, Action ())  -> [(Prop, Action ())]
applyToProp prop@(Assert _, _) = [prop]
applyToProp (prop@(Forall f), disp) = 
  [(f (unpack (constant x)), disp >> display_ x ", ") | x <- [0 .. (getMaxInegerSize prop)]]
applyToProp (prop@(ForallList maxLength f), disp) = 
  [(f (map (\x -> (unpack (constant x))) xs), disp >> display_ (fshowList xs) ", ") | xs <- (genListInputs (getMaxInegerSize prop) maxLength)]

getMaxInegerSize :: Prop -> Integer
getMaxInegerSize prop = 2 ^ (sizeProp prop) - 1

sizeProp :: Prop -> Int
sizeProp (Assert _) = 1
sizeProp (Forall (_ :: a -> Prop)) = valueOf @(SizeOf a)
sizeProp (ForallList _ (_ :: [a] -> Prop)) = valueOf @(SizeOf a)

genInputs :: [(Prop, Action ())] -> [(Prop, Action ())]
genInputs [] = []
genInputs props@((Assert _, _):_) = props
genInputs props@((Forall _, _):_) = 
  genInputs (concatMap (applyToProp) props)
genInputs props@((ForallList _ _, _):_) = 
    genInputs (concatMap (applyToProp) props)

displayProps :: [(Prop, Action ())] -> Action ()
displayProps [] = finish
displayProps ((Assert p, disp):props) = do
  _ <- display_ "Test with inputs: "
  disp
  _ <- display "\tresult: " (p)
  displayProps props
displayProps _ = noAction

checkTwo :: Prop -> Action ()
checkTwo prop = do
  let initial = [(prop, noAction)]
  let checkCases = (genInputs initial)
  displayProps checkCases

check :: Prop -> Module()
check prop = do
  let testSeq = (checkTwo prop)

  --globalTime :: Reg (Bit 32) <- makeReg 0

  always do
    testSeq
    
    --globalTime <== globalTime.val + 1
    --display "Time: " (globalTime.val)



twoSort :: KnownNat n => (Bit n, Bit n) -> (Bit n, Bit n)
twoSort (a, b) = (b - a) + 5 .>. 5 ? ((a, b), (b, a))
--twoSort (a, b) = a .<. b ? ((a, b), (b, a))

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



top :: Module ()
top = do
  --let propSubComm = Forall \a -> Forall \b -> Assert ((a :: Bit 4)-b.==.b-a)
  let propSort = Forall \b -> ForallList 2 \a -> Assert (isSorted (sort (a::[Bit 4])) .&. b)
  check propSort


main :: IO ()
main = writeVerilogTop top "top" "Out-Verilog/"
