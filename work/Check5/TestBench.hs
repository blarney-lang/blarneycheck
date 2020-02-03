{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}

module Check5.TestBench where

import Blarney
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