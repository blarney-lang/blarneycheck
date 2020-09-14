import Blarney
import ActoraStack_Impl
import BlarneyCheck

-- Helper functions for register lists
-- ===============

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


-- Stack with same interface as Actora stack
-- Use list of registers instead of BRAM
-- ===============

makeStackSpec :: (SizedBits a, KnownNat n) => Module (Stack n a)
makeStackSpec = do
  -- Pointer into list of registers
  sp :: Reg (Bit n) <- makeReg 0
  spWire :: Wire (Bit n) <- makeWire 0
  let spCurr = spWire.active ? (spWire.val, sp.val)

  -- Values being pushed to stack
  val1 :: Wire a <- makeWire (unpack (constant 0))
  val2 :: Wire a <- makeWire (unpack (constant 0))

  -- List of registers, big enough to hold stack elements
  elems :: [Reg a] <- replicateM (2^valueOf @n) (makeReg dontCare)
  let elemByIdx = indexIntoList elems
  let firstPush = (val2.active ? (spCurr + 1, spCurr), val1.val)
      secondPush = (spCurr, val2.val)
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
        sp <== sp.val + 2
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
        -- Commented out version allowed copying from top two elements
      --maxSize <== (maxSize.val .<. stkGolden.size + 1 ? (stkGolden.size + 1, maxSize.val))
        topTwo <== topTwo.val + (topTwo.val .==. 2 ? (0, 1))
        maxSize <== maxSize.val + (topTwo.val .==. 2 ? (1, 0))
      incMaxSizeTwo = do
      --maxSize <== (maxSize.val .<. stkGolden.size + 2 ? (stkGolden.size + 2, maxSize.val))
        topTwo <== 2
        maxSize <== maxSize.val + (topTwo.val.zeroExtend)
      decMaxSize = \n -> do
      --noAction
        let topTwoProtect = topTwo.val.zeroExtend .>=. n
        topTwo <== (topTwoProtect ? (topTwo.val .-. n.lower, 0))
        maxSize <== maxSize.val - (topTwoProtect ? (0, n .-. topTwo.val.zeroExtend))

  let top1Eq = stkGolden.top1 .==. stkActora.top1
      top2Eq = stkGolden.top2 .==. stkActora.top2
      prop_Top1Eq = Assert' ((stkGolden.size .==. 0) .|. top1Eq) (display_ (stkGolden.top1) " v " (stkActora.top1))
      prop_Top2Eq = Assert' ((stkGolden.size .<=. 1) .|. top2Eq) (display_ (stkGolden.top2) " v " (stkActora.top2))
      prop_SizeEq = Assert' (stkGolden.size .==. stkActora.size) (display_ (stkGolden.size) " v " (stkActora.size))

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
  let popGuard = \n -> \p -> (n .>. 0) .&. (n .<=. stkGolden.size + p)
      prop_Pop = Forall \n -> WhenAction (popGuard n 0) do
        pop stkGolden n
        pop stkActora n
        decMaxSize n
      prop_PushPop = Forall \x -> Forall \n -> WhenAction (popGuard n 1) do
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

  _ <- check properties reset 2
  --estimateTestCaseCount properties 6

  return ()

-- Code generation
-- ===============

main :: IO ()
main = writeVerilogTop testBench "top" "ActoraStack-Verilog/"
