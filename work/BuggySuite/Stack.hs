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
makeStack :: Bits a => Int -> Module (Stack a)
makeStack logSize = do
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
  stackSpec :: Stack (Bit 3) <- makeStackSpec 10
  stack :: Stack (Bit 3) <- makeStack 10

  let prop_StackPush = ("Push", Forall \x -> WhenAction 1 ((stackSpec.push) x >> (stack.push) x))
  let prop_StackPop =  ("Pop", WhenAction (stackSpec.isEmpty.inv .&. stack.isEmpty.inv) ((stackSpec.pop) >> (stack.pop)))
  --let prop_StackPop =  ("Pop", WhenRecipe (stackSpec.isEmpty.inv .&. stack.isEmpty.inv) (Par [Action (stackSpec.pop), Action (stack.pop)]))
  let prop_StackPushPopNop = ("PushPop", Forall \x -> WhenRecipe (stack.isEmpty.inv) $ Seq [Action $ (stack.push) x, Action $ stack.pop])

  let prop_StackTopEq = ("StackTopEq", Assert (stackSpec.isEmpty .|. (stackSpec.top .==. stack.top)))

  let rst = (stackSpec.clear) >> (stack.clear)
  _ <- check rst [prop_StackTopEq, prop_StackPush, prop_StackPop, prop_StackPushPopNop] 5

  return ()

main :: IO ()
main = writeVerilogTop testBench "top" "Out-Verilog/"