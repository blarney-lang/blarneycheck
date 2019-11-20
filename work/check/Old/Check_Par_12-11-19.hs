-- Add list comprehension to simplify functions
-- Take integer depth to enable going to some depth (dont know how to get size of a)

{-# LANGUAGE GADTs #-}

import Blarney

data Prop where
  Assert :: (Bit 1) -> Prop
  Forall :: (Bits a, KnownNat (SizeOf a)) => (a -> Prop) -> Prop


applyToProp :: Integer -> Prop -> [Prop]
applyToProp n (Forall f) = map f (map (\x -> unpack (constant x)) [0 .. n])

genInputs :: Integer -> [Prop] -> [Prop]
genInputs _ [] = []
genInputs n ((Assert p):xs) = ((Assert p):xs)
genInputs n ((Forall f):xs) = 
  genInputs n (concat (map (applyToProp n) ((Forall f):xs)))

displayProp :: Prop -> Action ()
displayProp (Assert p) = do
  display "Test, result: " (p)

checkTwo :: Integer -> Prop -> [Action ()]
checkTwo depth prop = map displayProp (genInputs depth [prop])

checkThree :: Integer -> Prop -> [Action ()]
checkThree depth prop = (checkGenerate "" depth 0 prop)
    where
      -- String is just for debugging, Integer tracks what case we are on
      checkGenerate :: String -> Integer -> Integer -> Prop -> [Action ()]
      checkGenerate s _ _ (Assert prop) = [do
                                    display "Test" s ", result: " (prop)
                                    ]
      checkGenerate s maxDepth currDepth (Forall f)
        | currDepth >= maxDepth = appliedForall
        | otherwise             = appliedForall ++ checkGenerate s maxDepth (currDepth+1) (Forall f)
        where
          appliedForall = checkGenerate (s ++ " " ++ show currDepth) maxDepth 0 (f (unpack (constant currDepth)))

check :: Integer -> Prop -> Module()
check depth prop = do
  let testSeq = Par (map Action (checkTwo (2^3-1) prop))

  done <- run (reg 1 0) testSeq

  globalTime :: Reg (Bit 32) <- makeReg 0

  always do
    (when done finish)
    
    globalTime <== globalTime.val + 1
    display "Time: " (globalTime.val)


top :: Module ()
top = do
  let propSubComm = Forall \a -> Forall \b -> Assert ((a :: Bit 4)-b.==.b-a)
  check (2^3-1) propSubComm


main :: IO ()
main = writeVerilogTop top "top" "Out-Verilog/"
