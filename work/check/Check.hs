{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

import Blarney

data RegList a = Nil | Cons (Reg a) (RegList a)

data Prop where
  Assert :: (Bit 1) -> Prop
  Forall :: (Bits a, Num a) => (a -> Prop) -> Prop

calculateReqRegs :: Prop -> Int
calculateReqRegs (Assert _) = 0
calculateReqRegs (Forall f) = 1 + calculateReqRegs(f (fromInteger 0))
{-
applyToProp :: Integer -> Prop -> [Prop]
applyToProp n (Forall f) = map f (map fromInteger [0 .. n])

genInputs :: Integer -> [Prop] -> [Prop]
genInputs _ [] = []
genInputs n ((Assert p):xs) = ((Assert p):xs)
genInputs n ((Forall f):xs) = 
  genInputs n (concat (map (applyToProp n) ((Forall f):xs)))
-}
displayProp :: Prop -> Action ()
displayProp (Assert p) = do
  display "Test, result: " (p)
{-
checkTwo :: Integer -> Prop -> [Action ()]
checkTwo depth prop = map displayProp (genInputs depth [prop])
{-
checkThree :: Integer -> Prop a -> [Action ()]
checkThree depth prop = (checkGenerate "" depth 0 prop)
    where
      -- String is just for debugging, Integer tracks what case we are on
      checkGenerate :: String -> Integer -> Integer -> Prop a -> [Action ()]
      checkGenerate s _ _ (Assert prop) = [do
                                    display "Test" s ", result: " (prop)
                                    ]
      checkGenerate s maxDepth currDepth (Forall f)
        | currDepth >= maxDepth = appliedForall
        | otherwise             = appliedForall ++ checkGenerate s maxDepth (currDepth+1) (Forall f)
        where
          appliedForall = checkGenerate (s ++ " " ++ show currDepth) maxDepth 0 (f (fromInteger currDepth))
-}
-}
checkSeq :: Bits a => Prop -> [Module (Reg a)] -> Action ()
checkSeq prop@(Assert _) _ = displayProp prop
checkSeq (Forall f) (reg:regs) = checkSeq (f (reg.val)) regs
       
check :: Integer -> Prop -> Module()
check depth prop = do
  let regs :: [Module (Reg (Bit 4))] = (map makeReg (replicate (calculateReqRegs prop) 0))
  let testSeq = Action (checkSeq prop regs)
  --let testSeq = Par (map Action (checkTwo (2^3-1) prop))

  done <- run (reg 1 0) testSeq

  --let (regs :: [Reg (Bit 4)]) = (map makeReg (replicate (calculateReqRegs prop) 0))

  globalTime :: Reg (Bit 32) <- makeReg 0


  always do
    (when done finish)
    
    globalTime <== globalTime.val + 1
    display "Time: " (globalTime.val)


top :: Module ()
top = do
  let propSubComm = Forall \a -> Forall \b -> Assert ((a:: Bit 4)-(b:: Bit 4).==.b-a)
  check (2^3-1) propSubComm


main :: IO ()
main = writeVerilogTop top "top" "Out-Verilog/"
