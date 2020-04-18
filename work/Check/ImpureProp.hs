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
import Check.Utils

{-|
  Created for WhenActions Props, used as the interface to increase the depth of the sequential search
-}
data ImpureEdge = ImpureEdge
    -- Used to traverse from one depth to another.
    -- Must be called on all ImpureEdges, not just the one being executed,
    -- as all use a Queue to track the inputs at the current depth and so all
    -- need this Action to be called to proceed to the next depth
    -- First Bit 1 sets if this action should be displayed
    -- Second Bit 1 indicated if this edge should run it's impure Action
    -- (should only be set on one IpureEdge at a time)
    { increaseDepthDisplay :: Bit 1 -> Action()
    , execImpure :: Bit 1 -> Action()
    , depthIncDone :: Bit 1

    -- Used during Increment phase: Increment bit -> Reset bit -> IncAction
    , increaseDepthInc :: Bit 1 -> Bit 1 -> Action()
    , edgeExhaused :: Bit 1
    
    -- Usually includes enqueueing one extra element in depth queue
    , increaseMaxDepth :: Action ()
    }

propToIE :: Int -> Prop -> Bool -> Module(ImpureEdge)
propToIE _ (Assert _) _ = error "Assert in Impure Props"
propToIE _ (WhenAction guardBit impureAction) _ = 
    return ImpureEdge {
        increaseDepthDisplay = \disp -> do
          when (disp .&. guardBit.inv) (display_ "    \t[Prevented by guard bit]")
          when (disp) (display_ "\n")
      , execImpure = \exec -> do
          when (exec .&. guardBit) impureAction
      , depthIncDone = 1
      , increaseDepthInc = \_ -> \_ -> noAction
      , edgeExhaused = 1
      , increaseMaxDepth = noAction
    }
propToIE _ (WhenRecipe guardBit impureRecipe) _ = do
    executingEdge <- makeReg 0
    execEdge <- makeReg 0
    myEdgeDone <- run (execEdge.val) impureRecipe
    return ImpureEdge {
        increaseDepthDisplay = \disp -> do
          when (disp .&. guardBit.inv) (display_ "    \t[Prevented by guard bit]")
          when (disp) (display_ "\n")
      , execImpure = \exec -> do
          when (exec .&. guardBit .&. executingEdge.val.inv) do
            --when disp (display "Starting edge")
            executingEdge <== 1
            execEdge <== 1
          when (executingEdge.val) do
            --when (myEdgeDone.inv .&. disp) (display "Not ending edge")
            --when (disp .&. myEdgeDone) (display "Ending edge")
            executingEdge <== myEdgeDone.inv
            execEdge <== 0
      , depthIncDone = myEdgeDone .|. (executingEdge.val.inv .&. guardBit.inv)
      , increaseDepthInc = \_ -> \_ -> noAction
      , edgeExhaused = 1
      , increaseMaxDepth = noAction
    }
propToIE maxSeq (Forall f) inList = do
    currMaxDepthReg :: Reg (Bit 16) <- makeReg 0
    let useQueue = currMaxDepthReg.val .>. 1
    queueOfElementsAppliedAtDepths :: Bits a => Queue a <- makeSizedQueue maxSeq
    regOfElementApplied <- makeReg initial
    let currVal = unpack (mux useQueue (pack $ queueOfElementsAppliedAtDepths.first, pack $ regOfElementApplied.val))
    let cycleDeq = when useQueue (queueOfElementsAppliedAtDepths.deq)
    let cycleEnq = \newVal -> do {
      if useQueue then ((queueOfElementsAppliedAtDepths.enq) newVal)
      else do
        regOfElementApplied <== newVal
        queueOfElementsAppliedAtDepths.deq
        (queueOfElementsAppliedAtDepths.enq) initial -- Must initialise queue with initital values
    }

    let amFinal = isFinal currVal
    let cycleQueue = \inc -> \rst -> do {
      cycleDeq
    ; if (rst .|. (inc .&. amFinal)) then
        cycleEnq initial
      else
        if inc then
          cycleEnq (next currVal)
        else
          cycleEnq currVal
    }
    ie <- propToIE maxSeq (f currVal) inList
    return ImpureEdge {
        increaseDepthDisplay = \disp -> do
          when disp (display_ " " (pack currVal))
          (cycleQueue 0 0)
          (ie.increaseDepthDisplay) disp
      , execImpure = ie.execImpure
      , depthIncDone = ie.depthIncDone
      , increaseDepthInc = \inc -> \rst -> cycleQueue inc rst >> (ie.increaseDepthInc) (inc .&. amFinal) rst
      , edgeExhaused = amFinal .&. ie.edgeExhaused
      , increaseMaxDepth = do
          currMaxDepthReg <== currMaxDepthReg.val + 1
          cycleEnq initial
          ie.increaseMaxDepth
    }
propToIE maxSeq (ForallList 0 f) _ = do
  ie <- propToIE maxSeq (f []) False
  return ie { increaseDepthDisplay = \disp -> do
    when disp (display_ "]")
    (ie.increaseDepthDisplay) disp
  }
propToIE maxSeq (ForallList listLen f) inList = do
  ie <- propToIE maxSeq (Forall $ \x -> ForallList (listLen - 1) $ \xs -> f (x:xs)) True
  let delimiter = if inList then "," else " ["
  return ie { increaseDepthDisplay = \disp -> do
    when disp (display_ delimiter)
    (ie.increaseDepthDisplay) disp
  }

{-
instance ImpureProp (Bit 1, Recipe) where
  iPropToTB _ (guardAct, impureRecipe) = do
    executingEdge <- makeReg 0
    execEdge <- makeReg 0
    edgeDone <- run (execEdge.val) impureRecipe
    return ImpureEdge {
        increaseDepthExec = \disp -> \exec -> \_ -> do
          when (disp) (display "\t[Executed: " guardAct "]")
          when (exec .&. guardAct .&. executingEdge.val.inv) do
            display "Starting edge"
            executingEdge <== 1
            execEdge <== 1
          when (executingEdge.val) do
            when (edgeDone.inv) (display "Not ending edge")
            when edgeDone (display "Ending edge")
            executingEdge <== edgeDone.inv
            execEdge <== 0
      , depthIncDone = edgeDone .|. (guardAct.inv)
      , increaseDepthInc = \_ -> \_ -> noAction
      , edgeExhaused = 1
      , increaseMaxDepth = noAction
    }
instance (Generator a, ImpureProp b) => ImpureProp (Int, [a] -> b) where
  iPropToTB maxDepth (listLen, f) = do
    if (maxDepth == 0) then do
      let appliedVal = replicate listLen initial
      ie <- iPropToTB maxDepth (f appliedVal)
      return ImpureEdge {
          increaseDepthExec = \disp -> \exec -> do
            when disp (display_ (map pack appliedVal) " ")
            (ie.increaseDepthExec) disp exec
        , depthIncDone = ie.depthIncDone
        , increaseDepthInc = ie.increaseDepthInc
        , edgeExhaused = ie.edgeExhaused
        , increaseMaxDepth = ie.increaseMaxDepth
      }
    else do
      
-}




propsToEdgesWithSelect :: KnownNat n => Int -> [Property] -> Module(Bit n -> ImpureEdge)
propsToEdgesWithSelect maxSeq props = do
  allEdges <- propsToEdges props
  return (\idx -> 
    ImpureEdge {
      increaseDepthDisplay = sel idx (map increaseDepthDisplay allEdges)
    , execImpure = sel idx (map execImpure allEdges)
    , depthIncDone = sel idx (map depthIncDone allEdges)
    , increaseDepthInc = sel idx (map increaseDepthInc allEdges)
    , edgeExhaused = sel idx (map edgeExhaused allEdges)
    , increaseMaxDepth = sel idx (map increaseMaxDepth allEdges)
  })
  where propsToEdges [] = return []
        propsToEdges ((name, prop):xs) = do
          ie <- propToIE maxSeq prop False
          let edge = ImpureEdge {
                        increaseDepthDisplay = \disp -> do
                          when disp (display_ name)
                          (ie.increaseDepthDisplay) disp
                      , execImpure = ie.execImpure
                      , depthIncDone = ie.depthIncDone
                      , increaseDepthInc = ie.increaseDepthInc
                      , edgeExhaused = ie.edgeExhaused
                      , increaseMaxDepth = ie.increaseMaxDepth
                    }
          edges <- propsToEdges xs
          return (edge:edges)



makeImpureTestBench :: Int -> Action() -> [Property] -> Module(ImpureTestBench)
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
    runPureTests :: Wire (Bit 1) <- makeWire 0
    -- In increment phase stop incrementing as soon as
    -- we hit an edge that we can increment
    amIncrementing :: Reg (Bit 1) <- makeReg 1

    -- Set to 1 to indicate we have tested all at this max depth
    currMaxDepthDone :: Reg (Bit 1) <- makeReg 1

    -- Cycle depth to 0 before displaying failing sequence
    displayFailingEdges :: Reg (Bit 1) <- makeReg 0
    startedDisplayFailingEdges :: Reg (Bit 1) <- makeReg 0

    -- Is 1 when all at currDepth are tested (and should go back to testing the first prop)
    let currDepthDone = edges.edgeExhaused .&. (selectBits + 1 .>=. impureEdgesLenBit)
    -- Is 1 when we have reached the curr max depth
    let isAtFinalDepth = currDepth.val + 1 .>=. currMaxDepthReg.val
    -- What is the last tested depth
    let lastTestDepth = mux (currDepth.val .!=. 0) (currDepth.val, currMaxDepthReg.val)

    let shouldIncDepth = edges.depthIncDone .|. phase.val
    let incrementDepth = do {
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
    }

    return ImpureTestBench {
      runEdge = do
        if (currMaxDepthDone.val) then
          noAction
        else do
        --when ((currMaxDepthReg.val .>=. 2)) (display "Run edge at depth " (currDepth.val) " with choice " selectBits " and amInc " (amIncrementing.val))
        -- Check if currently running edge, then wait until done.
        -- Or if am finished incrementing and waiting for depth increase

        when shouldIncDepth do
          incrementDepth
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
          if (amIncrementing.val .&. currDepthDone) then do
            --when (currMaxDepthReg.val .>=. 4) 
            (cycleEnq 0)
          -- When edge is exhausted but I can still go to next edge
          else if (amIncrementing.val .&. edges.edgeExhaused) then do
            (cycleEnq $ selectBits + 1)
          else do
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
          when ((currDepth.val .>=. depthTestedTo.val) .&. shouldIncDepth) (runPureTests <== 1)
          (edges.execImpure) 1
          when shouldIncDepth do
            (edges.increaseDepthDisplay) 0
            (cycleEnq selectBits)
    , edgeDone = (runPureTests.val.old) .&. edges.depthIncDone
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
        when (edges.depthIncDone) do
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
    }