{-# LANGUAGE GADTs #-}

module Check5.Property where

import Blarney
import Blarney.Queue
import Check5.Generator

data PureTestBench = PureTestBench 
    { runTest :: Action ()
    , increment :: Action ()
    , isDone :: Bit 1
    , reset :: Action ()
    , failed :: Bit 1
    } deriving (Generic, Interface)

data CycleCommand = CycleCommand ((Bit 1) -> (Bit 1) -> (Bit 1) -> Action (), Bit 1, Action())
                            -- Run Impure -- Increment -- Reset --        -- isFull -- displayOnFail

data Prop where
  Assert :: String -> (Bit 1) -> Prop
  Forall :: Generator a => String -> (a -> Prop) -> Prop
  WhenAction :: String -> (Bit 1) -> Action() -> Prop


isAssertProp :: Prop -> Bool
isAssertProp (Assert _ _) = True
isAssertProp (WhenAction _ _ _) = False
isAssertProp (Forall _ f) = isAssertProp (f initial)

splitProps :: [Prop] -> ([Prop], [Prop])
splitProps [] = ([], [])
splitProps (prop:props) = if (isAssertProp prop) then (prop:assert, sideEffect) else (assert, prop:sideEffect)
  where (assert, sideEffect) = splitProps props


makeAssertTestBench :: String -> (Bit 1) -> Action() -> PureTestBench
makeAssertTestBench name result dispSetValues = 
  PureTestBench { runTest = when (inv result) (dispSetValues >> (display "Assert \"" name "\" failed."))
                , increment = noAction
                , isDone = constant 1
                , reset = noAction
                , failed = inv result
                }

combinePureTestBenches :: Module [PureTestBench] -> Module PureTestBench
combinePureTestBenches mtbs = do
  tbs <- mtbs
  return (combineTBs tbs)
  where
    combineTBs [] = error "No Assert Props given"
    combineTBs [tb] = tb
    combineTBs (tb:tbs) =
      PureTestBench { runTest = (when (inv (tb.isDone)) (tb.runTest)) >> (when (inv (tb2.isDone)) (tb2.runTest))
                    , increment = tb.increment >> tb2.increment
                    , isDone = tb.isDone .&. tb2.isDone
                    , reset = tb.reset >> tb2.reset
                    , failed = tb.failed .|. tb2.failed
                    }
        where tb2 = combineTBs tbs



displayVarAndAbove :: SizedBits a => (String, a) -> Action () -> Action ()
displayVarAndAbove (name, genVal) dispAbove = dispAbove >> display_ name "=" (pack genVal) ", "

createIncrementAction :: SizedBits a => ((Bit 1, Action()), Reg a) -> Action ()
createIncrementAction ((isDone, increment), register) = do
    if (isFinal $ register.val) then do
      if isDone
        then
          noAction
        else do
          (register <== initial)
          increment
    else do
      register <== (next $ register.val)


{-Create the base TestBench here-}
checkGenPure :: Prop -> Module (PureTestBench)
checkGenPure prop = case prop of
    (Assert _ _) -> cgp noAction prop
    _            -> cgp (display_ "Set ") prop
  where cgp dispValues (Assert name p) = do
          return (makeAssertTestBench name p dispValues)
        cgp dispValues (Forall name f) = do
          gen <- makeReg initial
          tb <- cgp (displayVarAndAbove (name, gen.val) dispValues) (f (gen.val))
          return PureTestBench { runTest = tb.runTest
                              , increment = createIncrementAction ((tb.isDone, tb.increment), gen)
                              , isDone = isFinal (gen.val) .&. tb.isDone
                              , reset = (gen <== initial) >> tb.reset
                              , failed = tb.failed
              }
        cgp _ (WhenAction _ _ _) = error "Impure in pure method"
  







                                                                        -- Init -- Increment -- OnFail -- allDone
makeImpureTestBench :: Int -> Bit 16 -> Action() -> [CycleCommand] -> Module(Action(), Action(), Action(), Bit 1)
makeImpureTestBench maxDepth currMaxDepth rst impureEdges = do
  let maxDepthSafe = max maxDepth 1
  let impureEdgesLenBit = (constant (toInteger (length impureEdges)))
  queueOfEdgesTaken :: Queue (Bit 16) <- makeSizedQueue maxDepthSafe
  currDepth :: Reg (Bit 16) <- makeReg 0
  incrementPhase :: Reg (Bit 1) <- makeReg 0
  amIncrementing :: Reg (Bit 1) <- makeReg 1

  allDone :: Reg (Bit 1) <- makeReg 0
  let incrementAction = do {
    if ((currMaxDepth .==. 0) .|. (impureEdgesLenBit .==. 0)) then do
      allDone <== 1
    else do
      if (currDepth.val + 1 .>=. currMaxDepth) then do
        incrementPhase <== (inv (incrementPhase.val))
        currDepth <== 0
        if (incrementPhase.val) then rst
        else (amIncrementing <== 1)
      else do
        currDepth <== (currDepth.val) + 1
      queueOfEdgesTaken.deq
      if(incrementPhase.val) then do
        if (amIncrementing.val) then do
          if (getCurrEdgeFull impureEdges (queueOfEdgesTaken.first)) then do
            cycleAllWithSelectIncAndRst impureEdges (queueOfEdgesTaken.first) 0 0 1
            if((queueOfEdgesTaken.first) + 1 .>=. impureEdgesLenBit) then do
              --display "Inc Phase final"
              when (currDepth.val + 1 .>=. currMaxDepth) (allDone <== 1) -- If am the final one then finish
              (queueOfEdgesTaken.enq) 0
            else do
              --display "Inc Phase final val"
              (queueOfEdgesTaken.enq) ((queueOfEdgesTaken.first) + 1)
              amIncrementing <== 0
          else do
            --display "Inc Phase inc val"
            cycleAllWithSelectIncAndRst impureEdges (queueOfEdgesTaken.first) 0 1 0
            (queueOfEdgesTaken.enq) (queueOfEdgesTaken.first)
            amIncrementing <== 0
        else do
          --display "Inc Phase no inc"
          cycleAllWithSelectIncAndRst impureEdges (queueOfEdgesTaken.first) 0 0 0
          (queueOfEdgesTaken.enq) (queueOfEdgesTaken.first)
      else do
        --display "Exec"
        cycleAllWithSelectIncAndRst impureEdges (queueOfEdgesTaken.first) 1 0 0
        (queueOfEdgesTaken.enq) (queueOfEdgesTaken.first)
  }
  displayFail :: Reg (Bit 1) <- makeReg 0
  let failAction = do {
    if (currDepth.val .>=. currMaxDepth) then do
      if(displayFail.val) then do
        allDone <== 1
      else do
        display "Impure actions taken:"
        displayFail <== 1
        currDepth <== 0
    else do
      queueOfEdgesTaken.deq
      (queueOfEdgesTaken.enq) (queueOfEdgesTaken.first)
      currDepth <== (currDepth.val) + 1
      cycleAllWithSelectIncAndRst impureEdges (queueOfEdgesTaken.first) 0 0 0
      when (displayFail.val) (displaySelectFail impureEdges (queueOfEdgesTaken.first))
  }
  return (((queueOfEdgesTaken.enq) 0) >> (incrementPhase <== 0) >> (allDone <== 0), incrementAction, failAction, allDone.val)
  
getCurrEdgeFull :: [CycleCommand] -> (Bit 16) -> Bit 1
getCurrEdgeFull impureEdges selectIndex = gcef impureEdges 0
  where gcef [] _ = 0
        gcef ((CycleCommand (_, full, _)):ies) currIndex = ((selectIndex .==. (constant currIndex)) .&. full) .|. (gcef ies (currIndex + 1))


cycleAllWithSelectIncAndRst :: [CycleCommand] -> (Bit 16) -> Bit 1 -> Bit 1 -> Bit 1 -> Action ()
cycleAllWithSelectIncAndRst impureEdges selectIndex exec inc rst = cawsiar impureEdges 0
  where cawsiar [] _ = noAction
        cawsiar ((CycleCommand (cycle, _, _)):ies) currIndex = cycle (isSelect .&. exec) (isSelect .&. inc) (isSelect .&. rst) >> (cawsiar ies (currIndex + 1))
          where isSelect = (selectIndex .==. (constant currIndex))

displaySelectFail :: [CycleCommand] -> (Bit 16) -> Action ()
displaySelectFail impureEdges selectIndex = dsf impureEdges 0
  where dsf [] _ = noAction
        dsf ((CycleCommand (_, _, failAct)):ies) currIndex = when isSelect failAct >> (dsf ies (currIndex + 1))
          where isSelect = (selectIndex .==. (constant currIndex))


makeBranchFromImpure :: Int -> Prop -> Module(Action (), CycleCommand)
makeBranchFromImpure _ (WhenAction name guard impureAction) = 
  return (noAction, CycleCommand (\exec -> \_ -> \_ -> do
    when (exec .&. guard) impureAction
    , 1,
    display name))
makeBranchFromImpure maxDepth (Forall name f) = do
  let maxDepthSafe = max maxDepth 1
  queueOfElementsAppliedAtDepths <- makeSizedQueue maxDepthSafe
  (init, CycleCommand (cycle, full, dispFail)) <- makeBranchFromImpure maxDepth (f (queueOfElementsAppliedAtDepths.first))
  return ((queueOfElementsAppliedAtDepths.enq) initial >> init,
    CycleCommand (\exec -> \inc -> \rst -> do
      queueOfElementsAppliedAtDepths.deq
      if inc then do
        if (isFinal (queueOfElementsAppliedAtDepths.first)) then do
          (queueOfElementsAppliedAtDepths.enq) initial
          (cycle exec inc rst)
        else do
          (queueOfElementsAppliedAtDepths.enq) (next (queueOfElementsAppliedAtDepths.first))
          (cycle exec 0 rst)
      else do
        (cycle exec inc rst)
        if rst then do
          (queueOfElementsAppliedAtDepths.enq) initial
        else do
          (queueOfElementsAppliedAtDepths.enq) (queueOfElementsAppliedAtDepths.first)
      , full .&. isFinal (queueOfElementsAppliedAtDepths.first),
      (display_ "Set " name "=" (pack (queueOfElementsAppliedAtDepths.first)) " & ") >> dispFail
        ))
makeBranchFromImpure _ (Assert _ _) = error "Pure in Impure method"