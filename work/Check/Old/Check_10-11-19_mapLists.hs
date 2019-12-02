{-# LANGUAGE GADTs #-}

import Blarney

data Prop where
  Assert :: (Bit 1) -> Prop
  Forall :: (Bits a, Num a) => (a -> Prop) -> Prop

genInputs :: Integer -> [Prop] -> [Prop] -> [Prop]
genInputs _ [] finalProps = finalProps
genInputs n ((Assert p):xs) finalProps = genInputs n xs ((Assert p):finalProps)
genInputs n ((Forall f):xs) finalProps = genInputs n (xs++(map f (map fromInteger [0 .. n]))) finalProps

displayProp :: Prop -> Action ()
displayProp (Assert p) = do
  display "Test, result: " (p)

checkTwo :: Integer -> Prop -> [Action ()]
checkTwo depth prop = map displayProp (genInputs depth [prop] [])

check :: Integer -> Prop -> [Action ()]
check depth prop = (checkGenerate "" depth 0 prop)
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
          appliedForall = checkGenerate (s ++ " " ++ show currDepth) maxDepth 0 (f (fromInteger currDepth))


top :: Module ()
top = do
  let propSubComm = Forall \a -> Forall \b -> Assert ((a:: Bit 4)-(b:: Bit 4).==.b-a)
  
  let testSeq = Par (map Action (checkTwo (2^3-1) propSubComm))
  done <- run (reg 1 0) testSeq

  globalTime :: Reg (Bit 32) <- makeReg 0

  always do
    (when done finish)
    
    globalTime <== globalTime.val + 1
    display "Time: " (globalTime.val)

main :: IO ()
main = writeVerilogTop top "top" "Out-Verilog/"
