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

    execImpure :: Bit 1 -> Action ()
  , incEdge :: Bit 1 -> Action ()
  , displayValue :: Bit 1 -> Action ()
  , doneExec :: Bit 1

  , edgeExhausted :: Bit 1 -- If the last value executed was final
  , execFinal :: Bit 1 -- If the value just executed was final
  
  -- Usually includes enqueueing one extra element in depth queue
  , incEdgeSeqLen :: Action ()
}

propToIE :: Int -> Prop -> Bool -> Module(ImpureEdge)
propToIE _ (Assert _) _ = error "Assert in Impure Props"
propToIE _ (WhenAction guardBit impureAction) _ = 
    return ImpureEdge {
        execImpure = \exec -> do
          --when (exec) (display_ "WA")
          when (exec .&. guardBit) impureAction
      , incEdge = \_ -> noAction
      , displayValue = \disp -> do
          when (disp .&. guardBit.inv .&. 0) (display_ "\t[Blocked by Guard]")
      , doneExec = 1
      , edgeExhausted = 1
      , execFinal = 1
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
        execImpure = \exec -> do
          --when (exec) (display_ "WR")
          when (exec .&. guardBit) do
            --display_ "^^"
            execEdge <== 1
      , incEdge = \_ -> noAction
      , displayValue = \disp -> do
          when (disp .&. guardBit.inv .&. 0) (display_ "\t[Blocked by Guard]")
      , doneExec = myEdgeDone .|. (executing.val.inv) .|. (guardBit.inv.old)
      , edgeExhausted = 1
      , execFinal = 1
      , incEdgeSeqLen = noAction
    }
propToIE maxSeqLen (Forall (f :: b -> Prop)) inList = liftNat ((maxSeqLen + 1).fromIntegral.(logBase 2.0).ceiling) $ \(_ :: Proxy n) -> do
    currSeqLen :: Reg (Bit n) <- makeReg 0

    let useQueue = if (maxSeqLen == 1) then 0 else currSeqLen.val .>. 1
    appliedValsQueue :: Queue b <- makeSizedQueue maxSeqLen
    appliedValReg :: Reg b <- makeReg initial -- Beware that tuples are not initialised properly
    incVal <- makeWire (dontCare :: Bit 1)
    let oldVal = useQueue ? (appliedValsQueue.first, appliedValReg.val)
        oldFinal = isFinal oldVal
        nextVal = oldFinal ? (initial, next oldVal)
        currVal = incVal.active ? (incVal.val ? (nextVal, oldVal), appliedValReg.val)

    let cycleDeq = when useQueue do appliedValsQueue.deq
    let cycleEnq = \newVal -> do
        when useQueue do (appliedValsQueue.enq) newVal
        appliedValReg <== newVal

    let cycleQueue = \inc -> do
        cycleDeq
        cycleEnq (inc ? (nextVal, oldVal))
        incVal <== inc
    
    ie <- propToIE maxSeqLen (f currVal) inList
    return ie {
        incEdge = \inc -> do
          cycleQueue inc
          (ie.incEdge) (inc .&. oldFinal)
      , displayValue = \disp -> do
          when disp (display_ " 0x" (pack currVal))
          (ie.displayValue) disp
      , edgeExhausted = oldFinal .&. ie.edgeExhausted
      , execFinal = isFinal currVal .&. ie.execFinal
      , incEdgeSeqLen = do
          currSeqLen <== currSeqLen.val + 1
          appliedValReg <== initial -- Fixes tuples not initialised properly
          when (useQueue) do cycleEnq currVal
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
    , incEdge = sel oldIdx (map incEdge allEdges)
    , displayValue = sel idx (map displayValue allEdges)
    , doneExec = sel idx (map doneExec allEdges)
    , edgeExhausted = sel oldIdx (map edgeExhausted allEdges)
    , execFinal = sel idx (map execFinal allEdges)
    , incEdgeSeqLen = sel idx (map incEdgeSeqLen allEdges)
  })
  where propsToEdges [] = return []
        propsToEdges ((name, prop):xs) = do
          ie <- propToIE maxSeqLen prop False
          edges <- propsToEdges xs
          let edge = ie { displayValue = \disp -> do
                          when disp (display_ name)
                          (ie.displayValue) disp }
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

    incNextDepth :: Reg (Bit 1) <- makeReg 0 -- Keep at 1 if next depth should also be incremented
    allEdgesFinal :: Reg (Bit 1) <- makeReg 1 -- Used to detect that all sequences of len 'currSeqLen' tested

    edgesTakenQueue :: Queue (Bit w) <- makeSizedQueue maxSeqLen
    edgeTakenReg :: Reg (Bit w) <- makeReg 0
    let oldEdge = useQueue ? (edgesTakenQueue.first, edgeTakenReg.val)
        oldFinal = oldEdge .==. edgesWidth
        nextEdge = oldFinal ? (0, oldEdge + 1)

    edgesWithSelect <- propsToEdgesWithSelect impureProps maxSeqLen
    let incEdgeIdx = failed.inv .&. incNextDepth.val .&. edge.edgeExhausted
        currEdge = incEdgeIdx ? (nextEdge, oldEdge)
        edge = edgesWithSelect oldEdge currEdge

    let cycleDeq = when useQueue do edgesTakenQueue.deq
    let cycleEnq = \newVal -> do
        when useQueue do (edgesTakenQueue.enq) newVal
        edgeTakenReg <== newVal

    let depthExhausted = incEdgeIdx .&. oldFinal
    let currFinal = currEdge .==. edgesWidth

    return ImpureTestBench {
      execImpureEdge = do
        when (edge.doneExec) do
          currDepth <== nextDepth

          cycleDeq
          cycleEnq currEdge

          --when failed (display_ "Revert:" revertEdge ", di:" (depthEdgesIncTo.val) ", ")
          --when (incNextDepth.val) (display_ "@" nextVal "@")
          (edge.incEdge) (failed.inv .&. incNextDepth.val)
          (edge.execImpure) 1
          (edge.displayValue) (failed .|. isDebug)
          
          incNextDepth <== depthExhausted
          --when isDebug do display_ " ###" (oldEdge) "-" (edge.edgeExhausted) "-" (currEdge) "-" (incNextDepth.val) "-" depthExhausted "### "
          allEdgesFinal <== edge.execFinal .&. currFinal .&. allEdgesFinal.val
    , finishedExec = edge.doneExec
    , sequenceDone = isAtFinalDepth
    , displaySeqLen = display_ "%0d" (currSeqLen.val)
    , reset = do
        rst
        currDepth <== 0
        -- Don't increment the first sequence of lengths 1 and 2, since they start at 0s, others start at final val so inc those
        incNextDepth <== (allEdgesFinal.val.inv .|. useQueue)
        allEdgesFinal <== 1
    -- When all possibilities to current depth exhausted
    -- depthDone = 1 & incMaxDepth must be called
    , incSeqLen = do -- TODO
        _ <- display_ "- All tests passed to depth %0d" (currSeqLen.val)
        -- Enqueue to queues
        edge.incEdgeSeqLen
        cycleEnq (edgeTakenReg.val)
        -- Initialise values
        currSeqLen <== currSeqLen.val + 1
    , allSeqExec = allEdgesFinal.val .&. isAtFinalDepth
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