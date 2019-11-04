import Blarney

data Prop = Assert (Bit 1) | Forall (Bit 2 -> Prop)

-- String is just for debugging, Integer tracks what case we are on
checkGenerate :: String -> Integer -> Integer -> Prop -> [Recipe]
checkGenerate s _ _ (Assert prop) = [Action do
                              display "Test" s ", result: " (prop)
                              ]
checkGenerate s maxDepth currDepth (Forall f)
  | currDepth >= maxDepth = appliedForall
  | otherwise             = checkGenerate s maxDepth (currDepth+1) (Forall f) ++ appliedForall
  where
    appliedForall = checkGenerate (s ++ " " ++ show currDepth) maxDepth 0 (f (constant currDepth))

check :: Integer -> Prop -> [Recipe]
check depth prop = (checkGenerate "" depth 0 prop)


top :: Module ()
top = do
  let propSubComm = Forall \a -> Forall \b -> Assert (a.-.b.==.b.-.a)
  
  let testSeq = Par (check (2^2-3) propSubComm)
  done <- run (reg 1 0) testSeq

  globalTime :: Reg (Bit 32) <- makeReg 0

  always do
    (when done finish)
    
    globalTime <== globalTime.val + 1
    display "Time: " (globalTime.val)

main :: IO ()
main = writeVerilogTop top "top" "Out-Verilog/"
