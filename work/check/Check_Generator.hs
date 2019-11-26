-- Generalized TestBench to only provide the required functionality
-- Learned more about GADT and constructing them

{-# LANGUAGE GADTs #-}

import Blarney
--import Series -- Doesnt work?

data TestBench where
  Empty :: Action () -> TestBench
  --      Display result
  Gen :: Action () -> Action () -> (Bit 1) -> TestBench
  --      Display     Increment    Is Done

data Prop where
  Assert :: (Bit 1) -> Prop
  Forall :: (Bits a, KnownNat (SizeOf a)) => String -> (a -> Prop) -> Prop

type Series a = Int -> [a]

(\/) :: Series a -> Series a -> Series a
s1 \/ s2 = \d -> s1 d ++ s2 d
(><) :: Series a -> Series b -> Series (a, b)
s1 >< s2 = \d -> [(x,y) | x <- s1 d, y <- s2 d]

forallInputs :: (Bits a, KnownNat (SizeOf a)) => [a]
forallInputs = map (\x -> (unpack (constant x))) (iterate (\x -> x+1) 0)
forallListInputs :: (Bits a, KnownNat (SizeOf a)) => [[a]]
forallListInputs = 

doActionList :: [Action ()] -> Action ()
doActionList xs = foldr (>>) noAction xs

extractDones :: [TestBench] -> [Bit 1]
extractDones [] = []
extractDones ((Empty _):gs) = (constant 1):(extractDones gs)
extractDones ((Gen _ _ done):gs) = done:(extractDones gs)

extractIncrements :: [TestBench] -> [Action ()]
extractIncrements [] = []
extractIncrements ((Empty _):gs) = (noAction):(extractIncrements gs)
extractIncrements ((Gen _ increment _):gs) = increment:(extractIncrements gs)

{-Increment action generating functions-}
incReg :: (Bits a, KnownNat (SizeOf a)) => a -> a
incReg x = unpack ((pack x) + 1)

createIncrementAction :: (Bits a, KnownNat (SizeOf a)) => (TestBench, Reg a) -> Action ()
createIncrementAction ((Empty _), reg) = do
  if (reg.val === ones)
    then do
      noAction
    else
      (reg <== (incReg (reg.val)))
createIncrementAction ((Gen _ increment isDone), reg) = do
    if (reg.val === ones)
      then 
        if isDone 
          then
            noAction
          else do
            (reg <== (unpack 0))
            increment
      else
        (reg <== (incReg (reg.val)))

{-Display action generating functions-}
displayProp :: TestBench -> Action ()
displayProp (Empty disp) = disp
displayProp (Gen disp _ _) = disp

displayVarAndBelow :: (Bits a, KnownNat (SizeOf a)) => String -> (TestBench, Reg a) -> Action ()
displayVarAndBelow name (g, x) = do
  display "Set " name " to " (pack (x.val)) " "
  displayProp g


{-Create the base TestBench here-}
checkGen :: Prop -> Module (TestBench)
checkGen (Assert p) = do
  return (Empty (display "Test result: " p))
checkGen (Forall name f) = do
  inputs <- mapM makeReg (map unpack [0])
  gs <- mapM checkGen (map (\r -> f (r.val)) inputs)
  let combined = zip gs inputs
  let displayValue = doActionList (map (displayVarAndBelow name) combined)
  let isDone = andList ((map (\input -> (input.val === ones)) inputs)++(extractDones gs))
  let increment = if isDone then noAction else (doActionList (map createIncrementAction combined))
  return (Gen displayValue increment isDone)



isFinished :: TestBench -> Bit 1
isFinished (Empty _) = 1
isFinished (Gen _ _ isDone) = isDone


incrementGen :: TestBench -> Action ()
incrementGen (Empty _) = noAction
incrementGen (Gen _ increment _) = increment

check :: Prop -> Module(Bit 1)
check prop = do
  g <- (checkGen prop)

  globalTime :: Reg (Bit 32) <- makeReg 0
  testComplete :: Reg (Bit 1) <- makeReg 0
  always do
    --(when (globalTime.val .==. 2000) finish)
    globalTime <== globalTime.val + 1
    when (inv (testComplete.val)) do
      testComplete <== isFinished g
      incrementGen g
      displayProp g
    --display "Time: " (globalTime.val)
  return (testComplete.val)



firstHot :: KnownNat n => Bit n -> Bit n
firstHot x = x .&. ((inv x) .+. 1);

top :: Module ()
top = do
  let propSubComm = Forall "A" \a -> Forall "B" \b -> Assert ((a :: Bit 2)-b.==.b-a)
  --let prop_OneIsHot = Forall \x -> Assert (countOnes (firstHot (x :: Bit 4)) .==. ((x .==. 0) ? (0, 1)))
  --let prop_HotBitCommon = Forall \x -> Assert ((x :: Bit 4) .&. (firstHot x) .==. (firstHot x))
  --let prop_HotBitFirst = Forall \x -> Assert ((x :: Bit 4) .&. ((firstHot x) - 1) .==. 0)
  
  --t1Complete <- check prop_OneIsHot
  --t2Complete <- check prop_HotBitCommon
  --t3Complete <- check prop_HotBitFirst
  subComplete <- check propSubComm

  --let complete = t1Complete .&. t2Complete .&. t3Complete
  let complete = subComplete
  
  always do
    when complete do
      finish


main :: IO ()
main = writeVerilogTop top "top" "Out-Verilog/"
