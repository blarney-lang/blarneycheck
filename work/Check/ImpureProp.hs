{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}

module Check.ImpureProp where

import Blarney
import Blarney.Recipe
import Blarney.Queue
import Blarney.Core.Utils
import Check.Generator
import Check.Property
import Check.TestBench

-- Standard imports
import Data.Proxy


propToImpureTB :: Int -> Prop -> Bit 1 -> Bool -> Module(ImpureTestBench)
propToImpureTB _ (Assert _) _ _ = error "Assert in Impure Props"
propToImpureTB _ (Assert' _ _) _ _ = error "Assert in Impure Props"
propToImpureTB _ (WhenAction guardBit impureAction) _ _ = 
    return ImpureTestBench {
        execImpure = \exec -> do
          --when (exec) (display_ "WA")
          when (exec .&. guardBit) impureAction
      , incEdge = \_ -> noAction
      , displayValue = \disp -> do
          when (disp .&. guardBit.inv) (display_ "\t[Blocked by Guard]")
      , doneExec = 1
      , edgeExhausted = 1
      , execFinal = 1
      , incEdgeSeqLen = noAction
    }
propToImpureTB _ (WhenRecipe guardBit impureRecipe) _ _ = do
    execEdge <- makeWire 0
    executing <- makeReg 0
    myEdgeDone <- run (execEdge.val) impureRecipe
    always do
      when (execEdge.val) (executing <== 1)
      when (myEdgeDone) (executing <== 0)
    return ImpureTestBench {
        execImpure = \exec -> do
          --when (exec) (display_ "WR")
          when (exec .&. guardBit) do
            --display_ "^^"
            execEdge <== 1
      , incEdge = \_ -> noAction
      , displayValue = \disp -> do
          when (disp .&. guardBit.inv) (display_ "\t[Blocked by Guard]")
      , doneExec = myEdgeDone .|. (executing.val.inv) .|. (guardBit.inv.old)
      , edgeExhausted = 1
      , execFinal = 1
      , incEdgeSeqLen = noAction
    }
propToImpureTB maxSeqLen (Forall (f :: b -> Prop)) useQueue inList = do
    appliedValsQueue :: Queue b <- makeSizedQueue maxSeqLen
    appliedValReg :: Reg b <- makeReg initial -- Beware that tuples are not initialised properly
    incVal <- makeWire (dontCare :: Bit 1)
    let oldVal = useQueue ? (appliedValsQueue.first, appliedValReg.val)
        oldFinal = isFinal oldVal
        nextVal = oldFinal ? (initial, next oldVal)
        currVal = incVal.active ? (incVal.val ? (nextVal, oldVal), appliedValReg.val)

    let cycleDeq = when useQueue do deq appliedValsQueue
    let cycleEnq = \newVal -> do
        when useQueue do enq appliedValsQueue newVal
        appliedValReg <== newVal

    let cycleQueue = \inc -> do
        cycleDeq
        cycleEnq (inc ? (nextVal, oldVal))
        incVal <== inc

    ie <- propToImpureTB maxSeqLen (f currVal) useQueue inList
    return ie {
        incEdge = \inc -> do
          cycleQueue inc
          incEdge ie (inc .&. oldFinal)
      , displayValue = \disp -> do
          when disp (display_ " 0x" (pack currVal))
          displayValue ie disp
      , edgeExhausted = oldFinal .&. ie.edgeExhausted
      , execFinal = isFinal currVal .&. ie.execFinal
      , incEdgeSeqLen = do
          appliedValReg <== initial -- Fixes tuples not initialised properly
          enq appliedValsQueue initial
          incEdgeSeqLen ie
    }
propToImpureTB maxSeqLen (ForallList 0 f) useQueue _ = do
  ie <- propToImpureTB maxSeqLen (f []) useQueue False
  return ie { displayValue = \disp -> do
    when disp (display_ "]")
    displayValue ie disp
  }
propToImpureTB maxSeqLen (ForallList listLen f) useQueue inList = do
  ie <- propToImpureTB maxSeqLen (Forall $ \x -> ForallList (listLen - 1) $ \xs -> f (x:xs)) useQueue True
  let delimiter = if inList then "," else " ["
  return ie { displayValue = \disp -> do
    when disp (display_ delimiter)
    displayValue ie disp
  }

propsToEdgesWithSelect :: KnownNat n => [Property] -> Int -> Bit 1 -> Module(Bit n -> Bit n -> Bit n -> ImpureTestBench)
propsToEdgesWithSelect props maxSeqLen useQueue = do
  allEdges <- propsToEdges props
  return (\prevIdx -> \oldIdx -> \idx -> 
    ImpureTestBench {
      execImpure = sel idx (map execImpure allEdges)
    , incEdge = sel oldIdx (map incEdge allEdges)
    , displayValue = sel idx (map displayValue allEdges)
    , doneExec = sel prevIdx (map doneExec allEdges)
    , edgeExhausted = sel oldIdx (map edgeExhausted allEdges)
    , execFinal = sel idx (map execFinal allEdges)
    , incEdgeSeqLen = sel idx (map incEdgeSeqLen allEdges)
  })
  where propsToEdges [] = return []
        propsToEdges ((name, prop):xs) = do
          ie <- propToImpureTB maxSeqLen prop useQueue False
          edges <- propsToEdges xs
          let edge = ie { displayValue = \disp -> do
                          when disp (display_ name)
                          displayValue ie disp }
          return (edge:edges)





makeStatefulTester :: [Property] -> Action () -> Int -> Bit 1 -> Bit 1 -> Module(StatefulTester)
makeStatefulTester impureProps rst maxSeqLen testFailed isDebug =
  liftNat ((maxSeqLen + 1).log2ceil) $ \(_ :: Proxy h) -> 
  liftNat ((length impureProps + 1).log2ceil) $ \(_ :: Proxy w) -> do
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

    edgesWithSelect <- propsToEdgesWithSelect impureProps maxSeqLen useQueue
    let incEdgeIdx = testFailed.inv .&. incNextDepth.val .&. edge.edgeExhausted
        currEdge = incEdgeIdx ? (nextEdge, oldEdge)
        edge = edgesWithSelect (edgeTakenReg.val) oldEdge currEdge

    let cycleDeq = when useQueue do deq edgesTakenQueue
    let cycleEnq = \newVal -> do
        when useQueue do enq edgesTakenQueue newVal
        edgeTakenReg <== newVal

    let depthExhausted = incEdgeIdx .&. oldFinal
    let currFinal = currEdge .==. edgesWidth

    return StatefulTester {
      execImpureEdge = do
        when (edge.doneExec) do
          currDepth <== nextDepth

          cycleDeq
          cycleEnq currEdge

          incEdge edge (testFailed.inv .&. incNextDepth.val)
          execImpure edge true
          displayValue edge (testFailed .|. isDebug)
          
          incNextDepth <== depthExhausted
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
        incEdgeSeqLen edge
        enq edgesTakenQueue 0
        -- Initialise values
        currSeqLen <== currSeqLen.val + 1
    , allSeqExec = allEdgesFinal.val .&. isAtFinalDepth
    -- High when at the final seqence length
    , atMaxSeqLen = currSeqLen.val .==. maxSeqLen.toInteger.constant
  }
