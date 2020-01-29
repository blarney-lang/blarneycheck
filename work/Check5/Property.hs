{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}

module Check5.Property where

import Blarney
import Blarney.Queue
import Check5.Generator

data PureTestBench = PureTestBench 
    { increment :: Action ()
    , isDone :: Bit 1
    , reset :: Action ()
    , failed :: Bit 1
    , displayFailPure :: Action ()
    } deriving (Generic, Interface)

data ImpureTestBench = ImpureTestBench 
    -- Traverse one edge, until edgeDone is 1
    { runEdge :: Action ()
    , edgeDone :: Bit 1
    -- When all possibilities to current depth exhausted
    -- depthDone = 1 & incMaxDepth must be called
    , incMaxDepth :: Action ()
    , depthDone :: Bit 1
    -- Get the current max depth we are testing
    , currMaxDepth :: Bit 16
    -- Display last executed sequence, keep running until depthDone
    , displayFailImpure :: Action ()
    } deriving (Generic, Interface)

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
    { increaseDepthExec :: Bit 1 -> Bit 1 -> Action()
    , depthIncDone :: Bit 1

    -- Used during Increment phase: Increment bit -> Reset bit -> IncAction
    , increaseDepthInc :: Bit 1 -> Bit 1 -> Action()
    , edgeExhaused :: Bit 1
    
    -- Usually includes enqueueing one extra element in depth queue
    , increaseMaxDepth :: Action ()
    }

-- Given a Bit number (idx) selecting an element of [a] only select ([a])[idx]
class Selectable a where
  sel :: KnownNat n => Bit n -> [a] -> a

-- Always execute all actions, relevant Bits should already be selected
instance Selectable (Action ()) where
  sel _ actions = runAll actions
    where runAll [] = noAction
          runAll [a] = a
          runAll (a:as) = a >> (runAll as)

-- Only return true if selected bit is true
instance Selectable (Bit 1) where
  sel idx elems = selIdx elems 0
    where selIdx [] _ = 0
          selIdx (e:es) currIdx = (e .&. (idx .==. (constant currIdx))) .|. selIdx es (currIdx + 1)

-- Only apply true if the given bitVal is true and also only to elem[idx]
instance Selectable b => Selectable (Bit 1 -> b) where
  sel idx elems = \bitVal -> sel idx (applyIdx elems 0 bitVal)
    where applyIdx [] _ _ = []
          applyIdx (e:es) currIdx bitVal = do
            let appliedElem = e $ bitVal .&. (idx .==. (constant currIdx))
            let appliedAll = applyIdx es (currIdx + 1) bitVal
            appliedElem:appliedAll



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

splitProps :: [Prop] -> ([Prop], [Prop])
splitProps [] = ([], [])
splitProps ((prop@(Pure _ _)):props) = 
  let (assert, sideEffect) = splitProps props
  in (prop:assert, sideEffect)
splitProps ((prop@(Impure _ _)):props) =
  let (assert, sideEffect) = splitProps props
  in (assert, prop:sideEffect)


-- Class of Pure Props
class PureProp a where
  pPropToTB :: a -> Module(PureTestBench)

instance PureProp (Bit 1) where
  pPropToTB result = 
    return PureTestBench {
      increment = noAction
      , isDone = 1
      , reset = noAction
      , failed = inv result
      , displayFailPure = when (inv result) (display "failed! ***")
    }

instance (Generator a, PureProp b) => PureProp (a -> b) where
  pPropToTB f = do
    gen <- makeReg initial
    tb <- pPropToTB (f $ gen.val)
    let incrementAction = do {
      if (isFinal $ gen.val) then do
        if tb.isDone
          then
            noAction
          else do
            (gen <== initial)
            tb.increment
      else do
        gen <== (next $ gen.val)
    }
    return PureTestBench { 
      increment = incrementAction
      , isDone = isFinal (gen.val) .&. tb.isDone
      , reset = (gen <== initial) >> tb.reset
      , failed = tb.failed
      , displayFailPure = when (tb.failed) (display_ (pack $ gen.val) " ") >> (tb.displayFailPure)
    }
{-
instance (PureProp a) => PureProp [a] where
  pPropToTB [] = error "Must specify at least one Pure Property to test!"
  pPropToTB [x] = pPropToTB x
  pPropToTB (x:xs) = do
    tb <- pPropToTB x
    tbOthers <- pPropToTB xs
    return PureTestBench { increment = tb.increment >> tbOthers.increment
                         , isDone = tb.isDone .&. tbOthers.isDone
                         , reset = tb.reset >> tbOthers.reset
                         , failed = tb.failed .|. tbOthers.failed
                         , displayFailPure = tb.displayFailPure >> tbOthers.displayFailPure
                         }
-}

-- Class of Impure Props
class ImpureProp a where
  iPropToTB :: Int -> a -> Module(ImpureEdge)

instance ImpureProp (Bit 1, Action()) where
  iPropToTB _ (guardAct, impureAction) =
    return ImpureEdge {
        increaseDepthExec = \disp -> \exec -> do
          when (exec .&. guardAct) impureAction
          when (disp) (display "\t[Executed: " guardAct "]")
      , depthIncDone = 1
      , increaseDepthInc = \_ -> \_ -> noAction
      , edgeExhaused = 1
      , increaseMaxDepth = noAction
    }

instance (Generator a, ImpureProp b) => ImpureProp (a -> b) where
  iPropToTB maxDepth f =
    if (maxDepth == 0) then do
      let appliedVal = initial
      ie <- iPropToTB maxDepth (f appliedVal)
      return ImpureEdge {
          increaseDepthExec = \disp -> \exec -> do
            when disp (display_ (pack appliedVal) " ")
            (ie.increaseDepthExec) disp exec
        , depthIncDone = ie.depthIncDone
        , increaseDepthInc = ie.increaseDepthInc
        , edgeExhaused = ie.edgeExhaused
        , increaseMaxDepth = ie.increaseMaxDepth
      }
    else do
      currMaxDepthReg :: Reg (Bit 16) <- makeReg 0
      let useQueue = currMaxDepthReg.val .>. 1
      queueOfElementsAppliedAtDepths :: Bits a => Queue a <- makeSizedQueue maxDepth
      regOfEdgeTaken <- makeReg initial
      let currVal = unpack (mux useQueue (pack $ queueOfElementsAppliedAtDepths.first, pack $ regOfEdgeTaken.val))
      let cycleDeq = when useQueue (queueOfElementsAppliedAtDepths.deq)
      let cycleEnq = \newVal -> do {
        if useQueue then ((queueOfElementsAppliedAtDepths.enq) newVal)
        else (regOfEdgeTaken <== newVal)
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
      ie <- iPropToTB maxDepth (f currVal)
      return ImpureEdge {
          increaseDepthExec = \disp -> \exec -> do
            when disp (display_ (pack currVal) " ")
            cycleQueue 0 0
            (ie.increaseDepthExec) disp exec
        , depthIncDone = ie.depthIncDone
        , increaseDepthInc = \inc -> \rst -> cycleQueue inc rst >> (ie.increaseDepthInc) (inc .&. amFinal) rst
        , edgeExhaused = amFinal .&. ie.edgeExhaused
        , increaseMaxDepth = do
            currMaxDepthReg <== currMaxDepthReg.val + 1
            cycleEnq initial
            ie.increaseMaxDepth
      }























purePropToTB :: Prop -> Module PureTestBench
purePropToTB (Impure _ _) = error "Impure in pure props"
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
    , currMaxDepth = 0
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
    return ImpureTestBench {
      runEdge = do
        -- Check if currently running edge, then wait until done.
        -- Or if am finished incrementing and waiting for depth increase
        if (edges.depthIncDone.inv .|. currMaxDepthDone.val) then
          noAction
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
          --display "^ExecPhase"
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
            display "Impure actions taken (%0d):" (depthTestedTo.val)
        else
          (currDepth <== (currDepth.val) + 1)
        
        startedDisplayFailingEdges <== 1
        when (startedDisplayFailingEdges.val.inv) do
          if (currDepth.val .==. 0) then
            (depthTestedTo <== currMaxDepthReg.val)
          else
            (depthTestedTo <== currDepth.val)
        
        -- Display failing edge if have reset back to depth 0
        (edges.increaseDepthExec) (displayFailingEdges.val) (displayFailingEdges.val)
        -- Always cycle edges taken, and edges
        cycleDeq
        cycleEnq selectBits
        (edges.increaseDepthInc) 0 0
    }