{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}

module Check.ImpureProp where

import Blarney
import Blarney.Recipe
import Blarney.Queue
import Check.Generator
import Check.Property
import Check.TestBench

-- Standard imports
import Data.Proxy

{-|
  Created for WhenActions Props, used as the interface to increase the depth of the sequential search
-}
data ImpureEdge = ImpureEdge { 
  -- Used to traverse from one depth to another.
  -- Must be called on all ImpureEdges, not just the one being executed,
  -- as all use a Queue to track the inputs at the current depth and so all
  -- need this Action to be called to proceed to the next depth
  -- First Bit 1 sets if this action should be displayed
  -- Second Bit 1 indicated if this edge should run it's impure Action
  -- (should only be set on one IpureEdge at a time)

  --    Decrement   Increment  Execute
    execImpure :: Bit 1 -> Bit 1 -> Bit 1 -> Action ()
  , displayValue :: Bit 1 -> Action ()
  , doneExec :: Bit 1

  , edgeExhausted :: Bit 1
  
  -- Usually includes enqueueing one extra element in depth queue
  , incEdgeSeqLen :: Action ()
}

propToIE :: Int -> Prop -> Bool -> Module(ImpureEdge)
propToIE _ (Assert _) _ = error "Assert in Impure Props"
propToIE _ (WhenAction guardBit impureAction) _ = 
    return ImpureEdge {
        execImpure = \_ -> \_ -> \exec -> do
          --when (exec) (display_ "WA")
          when (exec .&. guardBit) impureAction
      , displayValue = \disp -> do
          when (disp .&. guardBit.inv) (display_ "\t[Blocked by Guard]")
      , doneExec = 1
      , edgeExhausted = 1
      , incEdgeSeqLen = noAction
    }
propToIE _ (WhenRecipe guardBit impureRecipe) _ = do
    execEdge <- makeWire 0
    executing <- makeReg 0
    myEdgeDone <- run (execEdge.val) impureRecipe
    always do
      when (execEdge.val) (executing <== 1)
      when (myEdgeDone) (executing <== 0)
    return ImpureEdge {
        execImpure = \_ -> \_ -> \exec -> do
          --when (exec) (display_ "WR")
          when (exec .&. guardBit) do
            --display_ "^^"
            execEdge <== 1
      , displayValue = \disp -> do
          when (disp .&. guardBit.inv) (display_ "\t[Blocked by Guard]")
      , doneExec = myEdgeDone .|. (executing.val.inv) .|. (guardBit.inv.old)
      , edgeExhausted = 1
      , incEdgeSeqLen = noAction
    }
propToIE maxSeqLen (Forall f) inList = liftNat ((maxSeqLen + 1).fromIntegral.(logBase 2.0).ceiling) $ \(_ :: Proxy n) -> do
    currSeqLen :: Reg (Bit n) <- makeReg 0
    init :: Reg (Bit 1) <- makeReg 0

    let useQueue = if (maxSeqLen == 1) then 0 else currSeqLen.val .>. 1
    prevAppliedValsQueue :: Bits a => Queue a <- makeSizedQueue maxSeqLen
    prevAppliedVal <- makeReg initial
    valAppliedReg <- makeReg initial
    appliedNow <- makeWire 0
    decrement <- makeWire 0
    let oldVal = useQueue ? (prevAppliedValsQueue.first, prevAppliedVal.val)

    let cycleDeq = prevAppliedValsQueue.deq
    let cycleEnq = \newVal -> do
        (prevAppliedValsQueue.enq) newVal
        prevAppliedVal <== newVal

    let oldFinal = isFinal oldVal
    let nextVal = (init.val .&. decrement.val.inv) ? (oldFinal ? (initial, next oldVal), oldVal)
    let valApplied = appliedNow.val ? (nextVal, valAppliedReg.val)
    let cycleQueue = \inc -> do
        cycleDeq
        if (inc) then do
          init <== 1
          cycleEnq nextVal
        else
          cycleEnq oldVal
    
    ie <- propToIE maxSeqLen (f valApplied) inList
    return ImpureEdge {
        execImpure = \dec -> \inc -> \exec -> do
          cycleQueue inc
          when (exec) do
            decrement <== dec
            valAppliedReg <== nextVal
            appliedNow <== 1
          (ie.execImpure) (dec .&. valApplied.isFinal) (inc .&. valApplied.isFinal) exec
      , displayValue = \disp -> do
          when disp (display_ " 0x" (pack valApplied))
          (ie.displayValue) disp
      , doneExec = ie.doneExec
      , edgeExhausted = valApplied.isFinal .&. ie.edgeExhausted
      , incEdgeSeqLen = do
          currSeqLen <== currSeqLen.val + 1
          cycleEnq (prevAppliedVal.val) -- Enqueue final value so that next value is initial
          when (currSeqLen.val .==. 1) cycleDeq
          ie.incEdgeSeqLen
    }
propToIE maxSeqLen (ForallList 0 f) _ = do
  ie <- propToIE maxSeqLen (f []) False
  return ie { displayValue = \disp -> do
    when disp (display_ "]")
    (ie.displayValue) disp
  }
propToIE maxSeqLen (ForallList listLen f) inList = do
  ie <- propToIE maxSeqLen (Forall $ \x -> ForallList (listLen - 1) $ \xs -> f (x:xs)) True
  let delimiter = if inList then "," else " ["
  return ie { displayValue = \disp -> do
    when disp (display_ delimiter)
    (ie.displayValue) disp
  }

propsToEdgesWithSelect :: KnownNat n => [Property] -> Int -> Module(Bit n -> Bit n -> ImpureEdge)
propsToEdgesWithSelect props maxSeqLen = do
  allEdges <- propsToEdges props
  return (\oldIdx -> \idx -> 
    ImpureEdge {
      execImpure = sel idx (map execImpure allEdges)
    , displayValue = sel idx (map displayValue allEdges)
    , doneExec = sel oldIdx (map doneExec allEdges)
    --, increaseDepthInc = sel idx_new (map increaseDepthInc allEdges)
    , edgeExhausted = sel idx (map edgeExhausted allEdges)
    , incEdgeSeqLen = sel idx (map incEdgeSeqLen allEdges)
  })
  where propsToEdges [] = return []
        propsToEdges ((name, prop):xs) = do
          ie <- propToIE maxSeqLen prop False
          let edge = ImpureEdge {
                        execImpure = ie.execImpure
                      , displayValue = \disp -> do
                          when disp (display_ name)
                          (ie.displayValue) disp
                      , doneExec = ie.doneExec
                      --, increaseDepthInc = ie.increaseDepthInc
                      , edgeExhausted = ie.edgeExhausted
                      , incEdgeSeqLen = ie.incEdgeSeqLen
                    }
          edges <- propsToEdges xs
          return (edge:edges)





makeImpureTestBench :: [Property] -> Action () -> Int -> Bit 1 -> Module(ImpureTestBench)
makeImpureTestBench impureProps rst maxSeqLen failed =
  liftNat ((maxSeqLen + 1).fromIntegral.(logBase 2.0).ceiling) $ \(_ :: Proxy h) -> 
  liftNat ((length impureProps + 1).fromIntegral.(logBase 2.0).ceiling) $ \(_ :: Proxy w) -> do
    let isDebug = testPlusArgs "DEBUG"
    let edgesWidth = (length impureProps - 1).toInteger.constant

    currDepth :: Reg (Bit h) <- makeReg 0
    let nextDepth = currDepth.val + 1

    currSeqLen :: Reg (Bit h) <- makeReg 0
    let useQueue = if (maxSeqLen == 1) then 0 else currSeqLen.val .>. 1
    -- Is 1 when we have reached the curr max depth
    let isAtFinalDepth = currDepth.val .==. currSeqLen.val

    incNextDepth :: Reg (Bit 1) <- makeReg 1

    queueOfEdgesTaken :: Queue (Bit w) <- makeSizedQueue maxSeqLen
    regOfEdgeTaken :: Reg (Bit w) <- makeReg edgesWidth
    depthIncTo :: Reg (Bit h) <- makeReg 0
    valAppliedReg <- makeReg 0
    let oldVal = useQueue ? (queueOfEdgesTaken.first, regOfEdgeTaken.val)

    let cycleDeq = queueOfEdgesTaken.deq
    let cycleEnq = \newVal -> do
        (queueOfEdgesTaken.enq) newVal
        regOfEdgeTaken <== newVal

    let oldFinal = oldVal .==. edgesWidth
    let nextValNoFail = oldFinal ? (0, oldVal + 1)
    let nextVal = (failed .&. incNextDepth.val .&. (depthIncTo.val .>. currDepth.val)) ? (oldVal, nextValNoFail)
    let cycleQueue = \inc -> do
        cycleDeq
        if (inc) then do
          cycleEnq nextVal
        else
          cycleEnq oldVal

    edgesWithSelect <- propsToEdgesWithSelect impureProps maxSeqLen

    let edge = edgesWithSelect (valAppliedReg.val) nextVal
    let depthExhausted = edge.edgeExhausted .&. (nextVal .==. edgesWidth)

    return ImpureTestBench {
      execImpureEdge = do
        when (edge.doneExec) do
          currDepth <== nextDepth

          --when failed (display_ "Revert:" revertEdge ", di:" (depthEdgesIncTo.val) ", ")
          --when (incNextDepth.val) (display_ "@" nextVal "@")
          (edge.execImpure) (failed .&. incNextDepth.val) (failed.inv .&. incNextDepth.val) 1 -- Writes current/next value to wire
          (edge.displayValue) (failed .|. isDebug)
          cycleQueue (edge.edgeExhausted .&. incNextDepth.val)
          valAppliedReg <== nextVal
          when (failed.inv) do
            if (incNextDepth.val .&. edge.edgeExhausted) then do
              depthIncTo <== nextDepth
            else do depthIncTo <== (depthIncTo.val .>. currDepth.val) ? (currDepth.val, depthIncTo.val)
          incNextDepth <== incNextDepth.val .&. edge.edgeExhausted .&. depthExhausted
    , finishedExec = edge.doneExec
    , sequenceDone = isAtFinalDepth
    , displaySeqLen = display_ "%0d" (currSeqLen.val)
    , reset = do
        rst
        currDepth <== 0
        incNextDepth <== 1
    -- When all possibilities to current depth exhausted
    -- depthDone = 1 & incMaxDepth must be called
    , incSeqLen = do -- TODO
        _ <- display_ "- All tests passed to depth %0d" (currSeqLen.val)
        -- Enqueue to queues
        edge.incEdgeSeqLen
        when (useQueue.inv) (cycleDeq)
        cycleEnq edgesWidth
        -- Initialise values
        currSeqLen <== currSeqLen.val + 1
    , allSeqExec = incNextDepth.val .&. isAtFinalDepth
    -- High when at the final seqence length
    , atMaxSeqLen = currSeqLen.val .==. maxSeqLen.toInteger.constant
    -- Get the current max depth we are testing
    {-, currMaxDepth = currSeqLen.val
    -- Display last executed sequence, keep running until depthDone
    , displayFailImpure = do
        -- If am finished displaying, skip
        if (currMaxDepthDone.val) then
          noAction
        else do
        if ((currDepth.val .>. depthTestedTo.val) .&. displayFailingEdges.val) then
          currMaxDepthDone <== 1
        else do
        when (edges.doneExec) do
          -- Display failing edge if have reset back to depth 0
          (edges.increaseDepthDisplay) (displayFailingEdges.val)

          cycleDeq
          cycleEnq selectBits
          if isAtFinalDepth then do
            if(displayFailingEdges.val) then do
              currMaxDepthDone <== 1
            else do
              rst
              displayFailingEdges <== 1
              currDepth <== 0
          else
            (currDepth <== (currDepth.val) + 1)
        
        when (startedDisplayFailingEdges.val.inv) do
          startedDisplayFailingEdges <== 1
          display "Impure actions taken (%0d):" lastTestDepth
        
        (edges.execImpure) 1
        -- (edges.increaseDepthExec) (displayFailingEdges.val) 0
        -- Always cycle edges taken, and edges
        -}
    }