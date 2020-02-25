{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ConstraintKinds #-}

import Blarney
import Blarney.Queue
import Blarney.Stream
import Check

testBench :: Stream (Bit 8) -> Module (Stream (Bit 8))
testBench bytesIn = do  
  bytesOut :: Queue (Bit 8) <- makeQueue
  let prop_Associativity = ("Associativity", Forall \(x :: Bit 4) -> Forall \(y :: Bit 4) -> Forall \(z :: Bit 4) -> Assert ((x .+. y) .+. z .==. x .+. (y .+. z)))
  let prop_Commutativity = ("Commutativity", Forall \(x :: Bit 4) -> Forall \(y :: Bit 4) -> Assert (x .+. y .==. y .+. x))

  _ <- check noAction [prop_Associativity, prop_Commutativity] 0 bytesOut

  always do
    when (bytesIn.canPeek) do
      bytesIn.consume

  return (bytesOut.toStream)


main :: IO ()
main = do
  writeVerilogModule testBench "Top" "Top-Verilog/"
