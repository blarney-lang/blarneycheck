-- Writing some nicer Haskell with list comprehension to generate list inputs.
-- But we still have a fixed sized bitvector input, want to generalise to Bits class - am struggling with Haskell types, want to avoid using extensions
-- Also want to separate input 'generation' from testing (also allows user to specify custom generators)
-- Possibly sequential testing to allow for synthesis.
-- Need for always passing around display action for the result is not nice.
-- successfully test a sorter network

import Blarney

data Prop = Assert (Bit 1) | Forall (Bit 8 -> Prop) | ForallList Integer ([Bit 1] -> Prop)

genListInputs :: Integer -> Integer -> [[Integer]]
genListInputs maxDepth currLength
  | currLength <= 0 = [[]]
  | otherwise       = []:[ x:xs | x <- [0 .. maxDepth], xs <- (genListInputs maxDepth (currLength-1))]

applyToProp :: (Prop, Action ())  -> [(Prop, Action ())]
applyToProp prop@(Assert _, _) = [prop]
applyToProp (prop@(Forall f), disp) = 
  [(f (unpack (constant x)), disp >> display_ x ", ") | x <- [0 .. 2^8-1]]
applyToProp (prop@(ForallList maxLength f), disp) = 
  [(f (map (\x -> (unpack (constant x))) xs), disp >> display_ (fshowList xs) ", ") | xs <- (genListInputs (2^1-1) maxLength)]



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
  always do
    testSeq


twoSort :: KnownNat n => (Bit n, Bit n) -> (Bit n, Bit n)
twoSort (a :: Bit n, b) = let halfSize = constant (toInteger (valueOf @(n))) in
                          (b - a) + halfSize .>=. halfSize ? ((a, b), (b, a))
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
  let propSort = ForallList 4 \a -> Assert (isSorted (sort a))
  check propSort


main :: IO ()
main = writeVerilogTop top "top" "Out-Verilog/"
