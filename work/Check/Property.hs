{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}

module Check.Property where

import Blarney
import Blarney.Queue
import Check.TestBench
import Check.PureProp
import Check.ImpureProp

{-
data Prop where
  Assert :: String -> (Bit 1) -> Action() -> Prop
  Forall :: Generator a => String -> (a -> Prop) -> Prop
  WhenAction :: String -> (Bit 1) -> Action() -> Prop
-}
-- All props must either be Pure or Impure
data Prop where
  Pure :: PureProp a => String -> a -> Prop
  Impure :: ImpureProp a => String -> a -> Prop



purePropToTB :: Prop -> Module PureTestBench
purePropToTB (Pure name prop) = do
  tb <- pPropToTB prop
  return PureTestBench { increment = tb.increment
                       , isDone = tb.isDone
                       , reset = tb.reset
                       , failed = tb.failed
                       , displayFailPure = when (tb.failed) (display_ "*** " name " ") >> (tb.displayFailPure)
                       }

combinePureProps :: [Prop] -> Module PureTestBench
combinePureProps pureProps = do
  combinePPs pureProps
  where
    combinePPs [] = error "No Assert Props given"
    combinePPs [prop] = purePropToTB prop
    combinePPs (prop:props) = do
      tb <- purePropToTB prop
      tbOthers <- combinePPs props
      return PureTestBench { increment = tb.increment >> tbOthers.increment
                           , isDone = tb.isDone .&. tbOthers.isDone
                           , reset = tb.reset >> tbOthers.reset
                           , failed = tb.failed .|. tbOthers.failed
                           , displayFailPure = tb.displayFailPure >> tbOthers.displayFailPure
                           }





propsToEdgesWithSelect :: KnownNat n => Int -> [Prop] -> Module(Bit n -> ImpureEdge)
propsToEdgesWithSelect maxDepth props = do
  allEdges <- propsToEdges props
  return (\idx -> 
    ImpureEdge {
      increaseDepthExec = sel idx (map increaseDepthExec allEdges)
    , depthIncDone = sel idx (map depthIncDone allEdges)
    , increaseDepthInc = sel idx (map increaseDepthInc allEdges)
    , edgeExhaused = sel idx (map edgeExhaused allEdges)
    , increaseMaxDepth = sel idx (map increaseMaxDepth allEdges)
  })
  where propsToEdges [] = return []
        propsToEdges ((Pure _ _):xs) = propsToEdges xs
        propsToEdges ((Impure name prop):xs) = do
          ie <- iPropToTB maxDepth prop
          let edge = ImpureEdge {
                        increaseDepthExec = \disp -> \exec -> do
                          when disp (display_ name " ")
                          (ie.increaseDepthExec) disp exec
                      , depthIncDone = ie.depthIncDone
                      , increaseDepthInc = ie.increaseDepthInc
                      , edgeExhaused = ie.edgeExhaused
                      , increaseMaxDepth = ie.increaseMaxDepth
                    }
          edges <- propsToEdges xs
          return (edge:edges)




makeImpureTestBench :: Int -> Action() -> [Prop] -> Module(ImpureTestBench)
makeImpureTestBench maxDepth rst impureProps = do
  if (maxDepth == 0 || length impureProps == 0) then
    return ImpureTestBench {
      runEdge = noAction
    , edgeDone = 1
    , incMaxDepth = noAction
    , depthDone = 1
    , currMaxDepth = constant (toInteger maxDepth)
    , displayFailImpure = noAction
    }
  else do
    let impureEdgesLenBit = (constant (toInteger (length impureProps)))

    currDepth <- makeReg 0
    depthTestedTo <- makeReg 0
    currMaxDepthReg <- makeReg 0
    let useQueue = currMaxDepthReg.val .>. 1

    queueOfEdgesTaken :: Queue (Bit 16) <- makeSizedQueue maxDepth
    regOfEdgeTaken :: Reg (Bit 16) <- makeReg 0
    let selectBits = mux useQueue (queueOfEdgesTaken.first, regOfEdgeTaken.val)
    let cycleDeq = when useQueue (queueOfEdgesTaken.deq)
    let cycleEnq = \newVal -> do {
      if useQueue then ((queueOfEdgesTaken.enq) newVal)
      else (regOfEdgeTaken <== newVal)
    }

    edgesWithSelect <- propsToEdgesWithSelect maxDepth impureProps
    let edges = edgesWithSelect selectBits



    -- 0 is execute phase, 1 is increment phase
    phase :: Reg (Bit 1) <- makeReg 0

    -- Set to 1 to run a round of Pure Prop testing before
    -- continuing to runEdge
    runPureTests :: Reg (Bit 1) <- makeReg 1
    -- In increment phase stop incrementing as soon as
    -- we hit an edge that we can increment
    amIncrementing :: Reg (Bit 1) <- makeReg 1

    -- Set to 1 to indicate we have tested all at this max depth
    currMaxDepthDone :: Reg (Bit 1) <- makeReg 1

    -- Cycle depth to 0 before displaying failing sequence
    displayFailingEdges :: Reg (Bit 1) <- makeReg 0
    startedDisplayFailingEdges :: Reg (Bit 1) <- makeReg 0

    -- Is 1 when all at currDepth are tested (and should go back to testing the first prop)
    let currDepthDone = selectBits + 1 .>=. impureEdgesLenBit
    -- Is 1 when we have reached the curr max depth
    let isAtFinalDepth = currDepth.val + 1 .>=. currMaxDepthReg.val
    -- What is the last tested depth
    let lastTestDepth = mux (currDepth.val .!=. 0) (currDepth.val, currMaxDepthReg.val)
    return ImpureTestBench {
      runEdge = do
        if (currMaxDepthDone.val) then
          noAction
        else do
        -- Check if currently running edge, then wait until done.
        -- Or if am finished incrementing and waiting for depth increase
        if (edges.depthIncDone.inv) then
          (edges.increaseDepthExec) 0 0
        else do
        -- If am at max depth then reset, otherwise increment
        -- IMPORTANT: inc or exec phase will still run after this
        if isAtFinalDepth then do
          phase <== phase.val.inv
          currDepth <== 0
          -- If switching to exec phase then reset state
          if (phase.val) then do
            rst
            when (depthTestedTo.val .==. 0) (runPureTests <== 1)
            --display("Reset")
          -- If switching to increment phase, start inc & disable testing
          else do
            amIncrementing <== 1
        else
          (currDepth <== (currDepth.val) + 1)
        -- Always want to cycle depth, since we increased it above
        cycleDeq
        -- Split on increment and execute phase
        if(phase.val) then do
          runPureTests <== 0
          --display "*IncPhase"
          -- If am still incrementing then increment edge if it isn't exhaused, otherwise reset it
          (edges.increaseDepthInc) (edges.edgeExhaused.inv .&. amIncrementing.val) (edges.edgeExhaused .&. amIncrementing.val)

          -- Cases for incrementing my current edge counter:
          -- When current edge is exhausted and am at the end of all edges, go back to 0
          if (amIncrementing.val .&. edges.edgeExhaused .&. currDepthDone) then
            (cycleEnq 0)
          -- When edge is exhausted but I can still go to next edge
          else if (amIncrementing.val .&. edges.edgeExhaused) then
            (cycleEnq $ selectBits + 1)
          else
          -- Edge is not exhausted so either got incremented or we aren't incrementing -> do nothing
            (cycleEnq selectBits)

          -- Since currDepthDone isn't done we mustv'e incremented an edge, so don't increment at next depth
          when (inv currDepthDone) (amIncrementing <== 0)
          
          -- If I've reached the current max depth, and incremented all of the edges here, but still want to increment then am done
          when (amIncrementing.val .&. currDepthDone .&. isAtFinalDepth) (currMaxDepthDone <== 1)
        else do
          --display "^ExecPhase" (currDepth.val)
          --(edges.displayEdge) 1
          -- Start Pure Tests when we are at an untested depth
          when (currDepth.val .>=. depthTestedTo.val) (runPureTests <== 1)
          (edges.increaseDepthExec) 0 1
          cycleEnq selectBits
    , edgeDone = (runPureTests.val) .&. edges.depthIncDone
    -- When all possibilities to current depth exhausted
    -- depthDone = 1 & incMaxDepth must be called
    , incMaxDepth = do
        -- Enqueue to queues
        edges.increaseMaxDepth
        cycleEnq 0
        -- Initialise values
        amIncrementing <== 0
        depthTestedTo <== currMaxDepthReg.val
        currMaxDepthReg <== currMaxDepthReg.val + 1
        currMaxDepthDone <== 0
    , depthDone = currMaxDepthDone.val
    -- Get the current max depth we are testing
    , currMaxDepth = currMaxDepthReg.val
    -- Display last executed sequence, keep running until depthDone
    , displayFailImpure = do
        -- If am finished displaying, skip
        if (currMaxDepthDone.val) then
          noAction
        else do
        if ((currDepth.val .>. depthTestedTo.val) .&. displayFailingEdges.val) then
          currMaxDepthDone <== 1
        else do
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
        
        -- Display failing edge if have reset back to depth 0
        (edges.increaseDepthExec) (displayFailingEdges.val) (displayFailingEdges.val)
        -- Always cycle edges taken, and edges
        cycleDeq
        cycleEnq selectBits
    }