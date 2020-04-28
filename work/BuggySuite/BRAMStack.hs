-- Blarney imports
import Blarney
import Blarney.Queue
import Blarney.Recipe
import Check.Check

-- Standard imports
import Data.Proxy

-- Stack interface
data Stack a =
  Stack {
    push    :: a -> Action ()
  , pop     :: Action ()
  , top     :: a
  , isEmpty :: Bit 1
  , clear   :: Action ()
  }

-- Buggy stack implementation
-- (Parallel push and pop not supported)
makeBRAMStack :: Bits a => Int -> Module (Stack a)
makeBRAMStack logSize = do
  -- Lift size to type-level number
  liftNat logSize $ \(_ :: Proxy n) -> do

    -- RAM, wide enough to hold entire stack
    ram :: RAM (Bit n) a <- makeDualRAMForward 0

    -- Stack pointer
    sp :: Reg (Bit n) <- makeReg 0

    -- Top stack element
    topReg :: Reg a <- makeReg dontCare

    -- Speculative read address
    speculateReg :: Reg (Bit n) <- makeReg 0
    speculateWire :: Wire (Bit n) <- makeWire (sp.val)

    -- Read top element from RAM
    always do
      load ram (speculateWire.active ? (speculateWire.val, speculateReg.val))
      when (speculateWire.active) do
        speculateReg <== speculateWire.val

    return $
      Stack {
        push = \a -> do
          topReg <== a
          store ram (sp.val) (topReg.val)
          speculateWire <== sp.val
          sp <== sp.val + 1
      , pop = do
          topReg <== ram.out
          speculateWire <== sp.val - 1  -- BUG: should be sp.val - 2
          sp <== sp.val - 1
      , top = topReg.val
      , isEmpty = sp.val .==. 0
      , clear = sp <== 0
      }

-- Stack specification
-- (Parallel push and pop not supported)
makeStackSpec :: Bits a => Int -> Module (Stack a)
makeStackSpec logSize =
  -- Lift size to type-level number
  liftNat logSize $ \(_ :: Proxy n) -> do

    -- List of register, big enough to hold stack elements
    elems :: [Reg a] <- replicateM (2^logSize) (makeReg dontCare)

    -- Size of stack
    size :: Reg (Bit n) <- makeReg 0

    return $
      Stack {
        push = \a -> do
          elems.head <== a
          zipWithM_ (<==) (tail elems) (map val elems)
          size <== size.val + 1
      , pop = do
          zipWithM_ (<==) elems (tail (map val elems))
          size <== size.val - 1
      , top = elems.head.val
      , isEmpty = size.val .==. 0
      , clear = size <== 0
      }

-- Top-level module
testBench :: Module ()
testBench = do
  stkGolden :: Stack (Bit 5) <- makeStackSpec 5
  stkBRAM :: Stack (Bit 5) <- makeBRAMStack 5

  let topEq = stkGolden.top .==. stkBRAM.top
  let prop_TopEq = Assert' (stkGolden.isEmpty .|. topEq) (display_ (stkGolden.top) " v " (stkBRAM.top))
  let prop_EmptyEq = Assert (stkGolden.isEmpty .==. stkBRAM.isEmpty)

  let prop_Push = Forall \x -> WhenAction true (push stkGolden x >> push stkBRAM x)
  let prop_Pop =  WhenAction (stkGolden.isEmpty.inv .&. stkBRAM.isEmpty.inv) (pop stkGolden >> pop stkBRAM)
  let prop_PushPopNop = Forall \x -> WhenRecipe true $ Seq [Action do push stkBRAM x, Action do pop stkBRAM]

  let properties = [
          ("TopEq", prop_TopEq)
        , ("EmptyEq", prop_EmptyEq)
        , ("Push", prop_Push)
        , ("Pop", prop_Pop)
        --, ("PushPop", prop_PushPopNop)
        ]
  let reset = stkGolden.clear >> stkBRAM.clear

  _ <- check properties reset 7
  --estimateTestCaseCount properties 7

  return ()

main :: IO ()
main = writeVerilogTop testBench "top" "Out-Verilog/"