import Blarney
import Blarney.Recipe
import CPU_Impl
import BlarneyCheck

-- Define custom type for store instructions
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
                      Forall \(StoreInstr si2 :: StoreInstr) -> Forall \(two :: Bit 1) -> WhenRecipe true do
                        Seq [ Action do (instr <== si1)
                            , Action do (instr <== 0b10 # branchOffset # si1.rD) -- si1.rD => Data hazard
                            , If two do Action (instr <== nop) -- Delay next instruction by either 0 or 1 clocks
                            , Action do (instr <== si2) -- Branched? => Control hazard 1 or 2 cycles after branch
                            , Action do
                                when (si1.imm .==. 0) do
                                  -- If didn't branch si2 must have an effect
                                  correctVal <== si2.imm.zeroExtend
                                  correctDest <== si2.rD
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
  
  -- Testing to depth 2 would take under 25mins on FPGA vs about 27hrs in simulation
  -- ~ (2^17)^2 * 10 clock cycles (~2^17 possibilities (branch), repeated twice, with avg. seq length of 10; 4.5 + 4.5 + 1)
  _ <- check properties reset 1
  --estimateTestCaseCount properties 2

  return ()

-- Code generation
main :: IO ()
main = writeVerilogTop testBench "top" "Out-Verilog/"
