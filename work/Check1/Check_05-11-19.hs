-- First focus on the most simple properties: forall bitvectors of constant size something must hold.
-- Initially an exercise of getting used to using blarney and haskell, so code quite clunky.
-- Check returns a list of Actions that check each enumeration, want to incorporate this checking into the test function.
-- Struggling with Haskell/Blarney monads
-- Successfully tested on first hot example

import Blarney
import Blarney.Recipe

data Prop = Assert (Bit 1) | Forall (Bit 8 -> Prop)

check :: Integer -> Prop -> [Action ()]
check depth prop = (checkGenerate "" depth 0 prop)
    where
      -- String is just for debugging, Integer tracks what case we are on
      checkGenerate :: String -> Integer -> Integer -> Prop -> [Action ()]
      checkGenerate s _ _ (Assert value) = [do
                                    display "Test" s ", result: " (value)
                                    ]
      checkGenerate s maxDepth currDepth (Forall f)
        | currDepth >= maxDepth = appliedForall
        | otherwise             = appliedForall ++ checkGenerate s maxDepth (currDepth+1) (Forall f)
        where
          appliedForall = checkGenerate (s ++ " " ++ show currDepth) maxDepth 0 (f (fromInteger currDepth))


firstHot :: Bit 8 -> Bit 8
firstHot x = x .&. ((inv x) .+. 1)

top :: Module ()
top = do
  let prop_HotBitCommon = Forall \x -> Assert (x .&. (firstHot x) .==. (firstHot x))
  
  let testSeq = Par (map Action (check (2^8-1) prop_HotBitCommon))
  done <- run (reg 1 0) testSeq

  globalTime :: Reg (Bit 32) <- makeReg 0

  always do
    (when done finish)
    
    globalTime <== globalTime.val + 1
    display "Time: " (globalTime.val)

main :: IO ()
main = writeVerilogTop top "top" "Out-Verilog/"
