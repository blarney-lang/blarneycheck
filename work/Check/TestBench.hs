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


data ImpureTestBench = ImpureTestBench { 
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
  -- High when at the final seqence length
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