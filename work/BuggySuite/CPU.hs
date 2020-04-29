-- Tiny 8-bit CPU with a 4-stage pipeline
--
-- Opcode    | Meaning
-- ----------+--------------------------------------------------------
-- 00DDNNNN  | Write value 0000NNNN to register DD
-- 01DDAABB  | Add register AA to register BB and store in register DD
-- 10NNNNBB  | Branch back by NNNN instructions if BB is non-zero
-- 1100NNNN  | NOP
-- 1111NNNN  | Halt

{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

import Blarney
import Blarney.Recipe
import Check.Check

-- Instructions
type Instr = Bit 8

-- Register identifiers
type RegId = Bit 2

-- Extract opcode
opcode :: Instr -> Bit 2
opcode instr = slice @7 @6 instr

-- Extract register A
rA :: Instr -> RegId
rA instr = slice @3 @2 instr

-- Extract register B
rB :: Instr -> RegId
rB instr = slice @1 @0 instr

-- Extract destination register
rD :: Instr -> RegId
rD instr = slice @5 @4 instr

-- Extract immediate
imm :: Instr -> Bit 4
imm instr = slice @3 @0 instr

-- Extract branch offset
offset :: Instr -> Bit 4
offset instr = slice @5 @2 instr

-- CPU
makeCPU :: Instr -> Module (RegId, Bit 8, Bit 1, Bit 8)
makeCPU instrMem = do
  -- Instruction memory
  -- instrMem :: RAM (Bit 8) Instr <- makeRAMInit "instrs.hex"

  -- Two block RAMs allows two operands to be read,
  -- and one result to be written, on every cycle
  regFileA :: RAM RegId (Bit 8) <- makeDualRAMForward 0
  regFileB :: RAM RegId (Bit 8) <- makeDualRAMForward 0

  -- Instruction register
  instr :: Reg (Bit 8) <- makeReg dontCare

  -- Instruction operand registers
  opA :: Reg (Bit 8) <- makeReg dontCare
  opB :: Reg (Bit 8) <- makeReg dontCare

  -- Program counter
  pcNext :: Wire (Bit 8) <- makeWire 0
  let pc = reg 0 (pcNext.val)

  -- Result of the execute stage
  result :: Wire (Bit 8) <- makeWire 0

  -- Wire to trigger a pipeline flush
  flush :: Wire (Bit 1) <- makeWire 0

  -- Cycle counter
  count :: Reg (Bit 32) <- makeReg 0
  always (count <== count.val + 1)

  -- Trigger for each pipeline stage
  go1 :: Reg (Bit 1) <- makeDReg 0
  go2 :: Reg (Bit 1) <- makeDReg 0
  go3 :: Reg (Bit 1) <- makeDReg 0

  always do
    -- Stage 0: Instruction Fetch
    -- ==========================

    -- Index the instruction memory
    -- load instrMem (pcNext.val)

    -- Start the pipeline after one cycle
    go1 <== 1

    -- Stage 1: Operand Fetch
    -- ======================

    when (go1.val) do
      when (flush.val.inv) do
        pcNext <== pc + 1
        go2 <== 1

    load regFileA (instrMem.rA)
    load regFileB (instrMem.rB)

    -- Stage 2: Latch Operands
    -- =======================
  
    -- Latch instruction
    instr <== instrMem.old

    -- Register forwarding logic
    let forward rS other =
          (result.active .&. (instr.val.rD .==. instrMem.old.rS)) ?
          (result.val, other)

    -- Latch operands
    opA <== forward rA (regFileA.out)
    opB <== forward rB (regFileB.out)

    -- Trigger stage 3
    when (flush.val.inv) do
      go3 <== go2.val

    -- Stage 3: Execute
    -- ================

    -- Instruction dispatch
    when (go3.val) do
      switch (instr.val.opcode)
        [
          -- Load-immediate instruction
          0b00 --> result <== zeroExtend (instr.val.imm),
          -- Add instruction
          0b01 --> result <== opA.val + opB.val,
          -- Branch instruction
          0b10 --> when (opB.val .!=. 0) do
                     pcNext <== pc - zeroExtend (instr.val.offset) - 2
                     -- Control hazard
                     flush <== 1,
          -- Halt instruction
          0b11 --> when (instr.val.rD .==. 0b11) finish
        ]

      -- Writeback
      when (result.active) do
        store regFileA (instr.val.rD) (result.val)
        store regFileB (instr.val.rD) (result.val)
        --display "%08d" (count.val) ": rf[%0d]" (instr.val.rD) " := 0x%02x" (result.val)
  return (instr.val.rD, result.val, result.active, pcNext.val)


-- Instruction Args
newtype StoreInstr = StoreInstr Instr deriving (Generic, Bits)

instance Generator StoreInstr where
  initial = unpack $ constant 0
  next current = unpack $ pack current + 1
  isFinal current = slice @5 @0 (pack current) .==. ones
  range = 2^6

testBench :: Module ()
testBench = do
  let nop = 0xc0
  instr :: Wire Instr <- makeWire nop
  correctVal :: Wire (Bit 8) <- makeWire 0
  correctDest :: Wire (Bit 2) <- makeWire 0
  correctPc :: Reg (Bit 8) <- makeReg 0
  (destination, value, writeBack, pc) <- makeCPU (instr.val)
  
  let prop_storeVal = Forall \(StoreInstr si :: StoreInstr) -> WhenRecipe true do
                        Seq [ Action do instr <== si
                            , Action do
                                -- Ensure that store results in correct value
                                -- getting written back to the correct register
                                correctVal <== si.imm.zeroExtend
                                correctDest <== si.rD
                                correctPc <== correctPc.val + 2
                            ]

  let prop_addVal = Forall \(StoreInstr si1 :: StoreInstr) -> Forall \(StoreInstr si2 :: StoreInstr) -> 
                      Forall \(dest :: RegId) -> WhenRecipe true do
                        Seq [ Action do (instr <== si1)
                            , Action do (instr <== si2)
                            , Action do (instr <== 0b01 # dest # si1.rD # si2.rD) -- Data hazard from arguments
                            , Action do
                                -- If si2 overwrote si1 then sum result should be 2*si2.imm, else si1.imm + si2.imm (zeroExtend first!)
                                correctVal <== si1.rD .==. si2.rD ? (2 * si2.imm.zeroExtend, si1.imm.zeroExtend + si2.imm.zeroExtend)
                                correctDest <== dest
                                correctPc <== correctPc.val + 4
                            ]

  let prop_hazard = Forall \(StoreInstr si1 :: StoreInstr) -> Forall \(branchOffset :: Bit 4) ->
                      Forall \(si2rD :: RegId) -> Forall \(two :: Bit 1) -> WhenRecipe true do
                        Seq [ Action do (instr <== si1)
                            , Action do (instr <== 0b10 # branchOffset # si1.rD) -- si1.rD => Data hazard
                            , If two do Action (instr <== nop) -- Delay next instruction by either 0 or 1 clocks
                            , Action do (instr <== 0b00 # si2rD # (0b1111 :: Bit 4)) -- Branched? => Control hazard 1 or 2 cycles after branch
                            , Action do
                                when (si1.imm .==. 0) do
                                  -- If didn't branch si2 must have an effect
                                  correctVal <== 0b1111
                                  correctDest <== si2rD
                                -- Took (4 + two) cycles to execute, however flush sets pc back by 3 cycles, also subtract jump
                                correctPc <== correctPc.val + two.zeroExtend + (si1.imm .==. 0 ? (4, 1 - branchOffset.zeroExtend))
                            ]

  let valActive = correctVal.active.(delay 0)
      prop_activeCorrect = Assert (valActive .==. writeBack)
      prop_valCorrect    = Assert (valActive.inv .|. (value .==. correctVal.val.old))
  let destActive = correctDest.active.(delay 0)
      prop_destCorrect = Assert  (destActive.inv .|. (destination .==. correctDest.val.old))
      prop_pcCorrect   = Assert' (correctPc.val .==. pc) (display_ (correctPc.val) " v " pc)
  let properties = [
        -- Should only be writing back after Store or Add
          ("Writeback_Correct", prop_activeCorrect)
        -- If is writing back, is that value correct
        , ("Value_Correct", prop_valCorrect)
        -- Is the register written back to the correct one
        , ("Destination_Correct", prop_destCorrect)
        -- Is the PC the correct value (important for branch)
        , ("PC_Correct", prop_pcCorrect)
        -- Store instruction followed by two NOPs
        -- Above properties checked during second NOP
        , ("Store", prop_storeVal)
        -- Two stores, then add, two NOPs and check
        , ("Add", prop_addVal)
        -- Store, then branch conditional
        -- Check that hazards are avoided
        , ("Branch", prop_hazard)
        ]
  let reset = correctPc <== pc + 1
  
  -- Testing to depth 2 would take under 2mins on FPGA vs about 50hrs in simulation
  -- ~ (2^15)^2 * 9.5 clock cycles (~2^15 possibilities, repeated twice, with avg. seq length of 9.5)
  _ <- check properties reset 1
  --estimateTestCaseCount properties 2

  return ()

-- Main function
main :: IO ()
main = writeVerilogTop testBench "top" "Out-Verilog/"
