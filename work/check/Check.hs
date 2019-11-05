import Blarney

data Prop = Assert (Bit 1) | Forall (Bit 3 -> Prop)

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
          appliedForall = checkGenerate (s ++ " " ++ show currDepth) maxDepth 0 (f (constant currDepth))


top :: Module ()
top = do
  let propSubComm = Forall \a -> Forall \b -> Assert (a.-.b.==.b.-.a)
  
  let testSeq = Par (map Action (check (2^3-1) propSubComm))
  done <- run (reg 1 0) testSeq

  globalTime :: Reg (Bit 32) <- makeReg 0

  always do
    (when done finish)
    
    globalTime <== globalTime.val + 1
    display "Time: " (globalTime.val)

main :: IO ()
main = writeVerilogTop top "top" "Out-Verilog/"
