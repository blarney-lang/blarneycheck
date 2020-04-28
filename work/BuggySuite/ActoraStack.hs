-- Standard imports
import Data.Proxy

-- This module implements a full-throughput dual-port stack.

import Blarney
import Blarney.Recipe
import Check.Check

-- Util
-- =========

-- Add and check for overflow
checkedAdd :: Bit n -> Bit n -> (Bit 1, Bit n)
checkedAdd a b = split result
  where
    z :: Bit 1 = 0
    result = (z # a) .+. (z # b)

-- Subtract and check for underflow
checkedSub :: Bit n -> Bit n -> (Bit 1, Bit n)
checkedSub a b = split result
  where
    z :: Bit 1 = 0
    result = (z # a) .-. (z # b)

-- Interface
-- =========

-- Stack of 2^n items of type a
data Stack n a =
  Stack {
    -- Can push one or two items per cycle:
    -- (push1 happens after push2)
    push1 :: a -> Action ()
  , push2 :: a -> Action ()
    -- Push nth item from top
    -- (n can be negative to implement a slide)
  , copy :: Bit n -> Action ()
    -- Pop any number of items
    -- (can be called in parallel with push1)
  , pop :: Bit n -> Action ()
    -- Size of the stack
  , size :: Bit n
    -- Top two stack values
  , top1 :: a
  , top2 :: a
    -- Watch for stack underflow/overflow
  , underflow :: Bit 1
  , overflow :: Bit 1
  , clear :: Action ()
  }

-- Implementation
-- ==============

makeStack :: (Bits a, KnownNat n) => Module (Stack n a)
makeStack = do
  -- True dual port RAM
  (ram1, ram2) <- makeTrueDualRAM

  -- Top two elements stored in registers
  reg1 :: Reg a <- makeReg dontCare
  reg2 :: Reg a <- makeReg dontCare

  -- Pointer to top of stack
  sp :: Reg (Bit n) <- makeReg 0

  -- Pointer plus one
  sp1 :: Reg (Bit n) <- makeReg 1

  -- When these signals are high, the RAM holds the
  -- top stack elements, not the registers
  unlatched1 :: Reg (Bit 1) <- makeDReg 0
  unlatched2 :: Reg (Bit 1) <- makeDReg 0
  let topVal1 = unlatched1.val ? (ram1.out, reg1.val)
  let topVal2 = unlatched2.val ? (ram2.out, reg2.val)

  -- Interface wires
  push1Wire <- makeWire dontCare
  push2Wire <- makeWire dontCare
  copyWire <- makeWire dontCare
  popWire <- makeWire 0
  let pushOrCopy = push1Wire.active .|. copyWire.active

  -- Watch for overflow/underflow
  let inc = push2Wire.active ? (2, pushOrCopy ? (1, 0))
  let (underflowFlag, spAfterPop) = (sp.val) `checkedSub` (popWire.val)
  let (overflowFlag, spNew) = spAfterPop `checkedAdd` inc

  always do
    -- Update stack pointer
    sp <== spNew
    sp1 <== (sp1.val - popWire.val) + inc

    -- Common address for ram1
    let addr1 = sp1.val - copyWire.val

    -- Pushing and not popping
    when (pushOrCopy .&. popWire.active.inv) do
      if push1Wire.active
        then reg1 <== push1Wire.val
        else do
          if copyWire.val .==. 0
            then reg1 <== topVal1
            else if copyWire.val .==. 1
                   then reg1 <== topVal2
                   else unlatched1 <== true
      if push2Wire.active
        then do
          reg2 <== push2Wire.val
          store ram1 addr1 topVal1
        else do
          reg2 <== topVal1
          load ram1 addr1
      store ram2 (sp.val) topVal2

    -- Popping and not pushing
    when (popWire.active .&. push1Wire.active.inv) do
      unlatched2 <== true
      load ram2 (sp.val - popWire.val)
      if popWire.val .==. 1
        then reg1 <== topVal2
        else unlatched1 <== true
      load ram1 (sp1.val - popWire.val)

    -- Pushing and popping
    when (push1Wire.active .&. popWire.active) do
      reg1 <== push1Wire.val
      reg2 <== topVal2
      when (popWire.val .!=. 1) do
        unlatched2 <== true
      load ram2 (sp1.val - popWire.val)

    -- Neither pushing, nor popping
    when (pushOrCopy.inv .&. popWire.active.inv) do
      reg1 <== topVal1
      reg2 <== topVal2

  return $
    Stack {
      push1 = \a -> push1Wire <== a
    , push2 = \a -> push2Wire <== a
    , copy = \n -> copyWire <== n
    , pop  = \n -> popWire <== n
    , size = sp.val
    , top1 = topVal1
    , top2 = topVal2
    , underflow = underflowFlag
    , overflow = underflowFlag.inv .&. overflowFlag
    , clear = popWire <== sp.val
    }


indexIntoList :: (KnownNat n, Bits b, KnownNat (SizeOf b)) => [Reg b] -> Bit n -> b
indexIntoList list idx = idxList 0 list
  where idxList _ [] = unpack (constant 0)
        idxList currIdx (x:xs) = (idx .==. currIdx) ? (x.val, idxList (currIdx + 1) xs)
assignIntoList :: (KnownNat n, Bits b) => [Reg b] -> (Bit n, b) -> (Bit n, b) -> Action ()
assignIntoList list (idx1, newVal1) (idx2, newVal2) = assignIdxList 0 list
  where assignIdxList _ [] = noAction
        assignIdxList currIdx (x:xs) = do
          x <== (idx1 .==. currIdx) ? (newVal1, (idx2 .==. currIdx) ? (newVal2, x.val))
          assignIdxList (currIdx + 1) xs

-- Stack specification
-- (Parallel push and pop not supported)
makeStackSpec :: (Bits a, KnownNat (SizeOf a), KnownNat n) => Module (Stack n a)
makeStackSpec = do
  -- Size of stack
  sp :: Reg (Bit n) <- makeReg 0
  spWire :: Wire (Bit n) <- makeWire 0
  let spCurr = spWire.active ? (spWire.val, sp.val)

  val1 :: Wire a <- makeWire (unpack (constant 0))
  val2 :: Wire a <- makeWire (unpack (constant 0))

  -- List of register, big enough to hold stack elements
  elems :: [Reg a] <- replicateM (2^valueOf @n) (makeReg dontCare)
  let elemByIdx = indexIntoList elems
  let firstPush = (spCurr, val2.active ? (val2.val, val1.val))
      secondPush = ((val1.active .&. val2.active) ? (spCurr + 1, spCurr), val1.val)
      assign = assignIntoList elems firstPush secondPush
  let push = \a -> do
        val1 <== a
        when (val2.active.inv) do
          assign
          when (spWire.active.inv) do sp <== sp.val + 1
  return $
    Stack {
      push1 = push
    , push2 = \b -> do
        val2 <== b
        assign
        sp <== sp.val + (val1.active ? (2, 1))
    , copy = \n -> push (elemByIdx $ sp.val - n - 1)
    , pop = \n -> do
        spWire <== sp.val - n
        sp <== sp.val - n + (val1.active ? (1,0))
    , size = sp.val
    , top1 = elemByIdx $ sp.val - 1
    , top2 = elemByIdx $ sp.val - 2
    , underflow = 0
    , overflow = 0
    , clear = do
        sp <== 0
  }

-- Test bench
-- ==========

testBench :: Module ()
testBench = do
  stkGolden :: Stack 3 (Bit 1) <- makeStackSpec
  stkActora :: Stack 3 (Bit 1) <- makeStack
  -- Note 3: Cannot copy top two elements from stack
  maxSize :: Reg (Bit 3) <- makeReg 0
  topTwo :: Reg (Bit 2) <- makeReg 0
  let incMaxSizeOne = do
        topTwo <== topTwo.val + (topTwo.val .==. 2 ? (0, 1))
        maxSize <== maxSize.val + (topTwo.val .==. 2 ? (1, 0))
      incMaxSizeTwo = do
        topTwo <== 2
        maxSize <== maxSize.val + (topTwo.val.zeroExtend)
      decMaxSize = \n -> do
        let topTwoProtect = topTwo.val.zeroExtend .>=. n
        topTwo <== (topTwoProtect ? (topTwo.val .-. n.lower, 0))
        maxSize <== maxSize.val - (topTwoProtect ? (0, n .-. topTwo.val.zeroExtend))

  let top1Eq = stkGolden.top1 .==. stkActora.top1
      top2Eq = stkGolden.top2 .==. stkActora.top2
      prop_Top1Eq = Assert' ((stkGolden.size .==. 0) .|. top1Eq) (display_ " ^^" (stkGolden.top1) "|" (stkActora.top1) "^^ ")
      prop_Top2Eq = Assert ((stkGolden.size .<=. 1) .|. top2Eq)
      prop_SizeEq = Assert (stkGolden.size .==. stkActora.size)

  let prop_Push1 = Forall \x -> WhenAction true (push1 stkGolden x >> push1 stkActora x >> incMaxSizeOne)
      --prop_Push2 = Forall \x -> WhenAction true (push2 stkGolden x >> push2 stkActora x >> incMaxSizeOne)
      prop_Push3 = Forall \x -> Forall \y -> WhenAction true do
        push1 stkGolden x
        push1 stkActora x
        push2 stkGolden y
        push2 stkActora y
        incMaxSizeTwo
      
  let copyGuard = \n -> (stkGolden.size .>. n) .|. -- If copying from stack, can only copy up to stk.size elements back
                        ((stkGolden.size .<. maxSize.val) .&. (n .>=. stkGolden.size - maxSize.val)) -- If copying forward
      prop_Copy = Forall \n -> WhenAction (copyGuard n) do
        copy stkGolden n
        copy stkActora n
        incMaxSizeOne

  -- Note 2: Cannot pop 0
  let popGuard = \n -> (n .>. 0) .&. (n .<=. stkGolden.size)
      prop_Pop = Forall \n -> WhenAction (popGuard n) do
        pop stkGolden n
        pop stkActora n
        decMaxSize n
      prop_PushPop = Forall \x -> Forall \n -> WhenAction (popGuard n) do
        pop stkGolden n
        pop stkActora n
        push1 stkGolden x
        push1 stkActora x
        decMaxSize (n - 1)

  let properties = [
          ("Top1Eq", prop_Top1Eq)
        , ("Top2Eq", prop_Top2Eq)
        , ("SizeEq", prop_SizeEq)
        , ("Push1", prop_Push1)
        --, ("Push2", prop_Push2) -- Note 1: cannot push2 without push1
        , ("Push1&2", prop_Push3)
        , ("Copy", prop_Copy)
        , ("Pop", prop_Pop)
        , ("PushPop", prop_PushPop)
        ]
  let reset = stkGolden.clear >> stkActora.clear >> (maxSize <== 0) >> (topTwo <== 0)

  _ <- check properties reset 4
  --estimateTestCaseCount properties 7

  return ()

-- Code generation
-- ===============

main :: IO ()
main = writeVerilogTop testBench "top" "Out-Verilog/"