{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}

module Check.TestBench where

import Blarney

data PureTestBench = PureTestBench { 
    increment :: Action ()
  , isDone :: Bit 1
  , failed :: Bit 1
  , displayFail :: Action ()
}


{-|
  Created for Impure Props, used as the interface to increase the depth of the sequential search
-}
data ImpureTestBench = ImpureTestBench { 
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


data StatefulTester = StatefulTester { 
  -- Execute WhenAction or WhenRecipe (traversing one edge)
  -- If Bit is high then execute with incremented value (normal execution)
  -- Else don't inc but display value (when printing failing case)
    execImpureEdge :: Action ()
  -- When high effects of Execution are visible -> can execute next edge or start pure testing
  , finishedExec :: Bit 1
  -- High when executed last element of sequence, must reset after this
  , sequenceDone :: Bit 1
  -- High when at the final seqence length
  , displaySeqLen :: Action ()
  -- When pure testing is done, reset to depth 0
  , reset :: Action ()
  -- When all possibilities to current sequence length exhausted
  -- allSeqExec will be high & incSeqLen must be called
  , incSeqLen :: Action ()
  , allSeqExec :: Bit 1
  -- High when at the final seqence length (when this and allSeqExec high then testing concludes)
  , atMaxSeqLen :: Bit 1
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
