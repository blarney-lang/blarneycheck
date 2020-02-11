{-# LANGUAGE GADTs #-}
{-# LANGUAGE DatatypeContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

import Blarney
import Check.Check

data SizedBits a => MemReq a = MemReq { memReq :: a }

instance SizedBits a => Generator (MemReq a) where
  initial = unpack (constant 10)
  next current = unpack $ (pack current) .+. 1
  isFinal current = (pack current) .==. ones

testBench :: Module ()
testBench = do
  --let propSubComm = Forall "A" \a -> Forall "B" \b -> Assert ((a :: Bit 2)-b.==.b-a)
  let prop_Associativity = ("Associativity", Forall \(x :: Bit 4) -> Forall \(y :: Bit 4) -> Forall \(z :: Bit 4) -> Assert ((x .+. y) .+. z .==. x .+. (y .+. z)))
  let prop_Commutativity = ("Commutativity", Forall \(x :: Bit 4) -> Forall \(y :: Bit 4) -> Assert (x .+. y .==. y .+. x))
  
  _ <- check noAction [prop_Associativity, prop_Commutativity] 0
  
  return ()

main :: IO ()
main = writeVerilogTop testBench "top" "Out-Verilog/"