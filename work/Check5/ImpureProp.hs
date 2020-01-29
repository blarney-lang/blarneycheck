{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}

module Check5.ImpureProp where

import Blarney
import Blarney.Queue
import Check5.Generator
import Check5.Property

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

doActionList :: [Action ()] -> Action()
doActionList as = foldr (>>) noAction as

incrementGenList :: Generator a => [a] -> [a]
incrementGenList as = igl 1 as
  where igl :: Generator a => Bit 1 -> [a] -> [a]
        igl _ [] = []
        igl inc (x:xs) =
          let incXs = igl (inc .&. isFinal x) xs
          in (unpack $ mux inc (mux (isFinal x) (initial, pack (next x)), pack x)):incXs

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
      currMaxDepthReg :: Reg (Bit 16) <- makeReg 0
      let useQueue = currMaxDepthReg.val .>. 1
      queueOfElementsAppliedAtDepths :: Bits a => [Queue a] <- mapM makeSizedQueue (replicate listLen maxDepth)
      regOfEdgeTaken :: Bits a => [Reg a] <- mapM makeReg (replicate listLen initial)
      let currVal = map (\(q, r) -> unpack (mux useQueue (pack $ q.first, pack $ r.val))) (zip queueOfElementsAppliedAtDepths regOfEdgeTaken)
      
      let cycleDeq = when useQueue (doActionList (map deq queueOfElementsAppliedAtDepths))
      let cycleEnq = \newVal -> do {
        if useQueue then (doActionList (map (\(q, nV) -> (q.enq) nV) (zip queueOfElementsAppliedAtDepths newVal)))
        else (doActionList (map (\(r, nV) -> r <== nV) (zip regOfEdgeTaken newVal)))
      }

      let amFinal = andList $ map isFinal currVal
      let cycleQueue = \inc -> \rst -> do {
        cycleDeq
      ; if (rst .|. (inc .&. amFinal)) then
          cycleEnq (replicate listLen initial)
        else
          if inc then
            cycleEnq (incrementGenList currVal)
          else
            cycleEnq currVal
      }
      ie <- iPropToTB maxDepth (f currVal)
      return ImpureEdge {
          increaseDepthExec = \disp -> \exec -> do
            when disp (display_ (map pack currVal) " ")
            cycleQueue 0 0
            (ie.increaseDepthExec) disp exec
        , depthIncDone = ie.depthIncDone
        , increaseDepthInc = \inc -> \rst -> cycleQueue inc rst >> (ie.increaseDepthInc) (inc .&. amFinal) rst
        , edgeExhaused = amFinal .&. ie.edgeExhaused
        , increaseMaxDepth = do
            currMaxDepthReg <== currMaxDepthReg.val + 1
            cycleEnq (replicate listLen initial)
            ie.increaseMaxDepth
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