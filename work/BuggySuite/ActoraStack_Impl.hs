module ActoraStack_Impl where

-- This module implements a full-throughput dual-port stack.

import Blarney

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
