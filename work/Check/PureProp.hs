{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}

module Check.PureProp where

import Blarney
import Check.TestBench
import Check.Generator



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

instance (Generator a, PureProp b) => PureProp (Int, [a] -> b) where
  pPropToTB (listLen, f) = do
    gens <- mapM makeReg (replicate listLen initial)
    let vals = map val gens
    let resetRegAction = doActionList (map (\(r, nV) -> r <== nV) (zip gens (replicate listLen initial)))
    let incRegAction = doActionList (map (\(r, nV) -> r <== nV) (zip gens (incrementGenList vals)))
    let amFinal = andList $ map isFinal vals
    tb <- pPropToTB (f vals)
    let incrementAction = do {
      if (amFinal) then do
        if tb.isDone
          then
            noAction
          else do
            resetRegAction
            tb.increment
      else do
        incRegAction
    }
    return PureTestBench { 
      increment = incrementAction
      , isDone = amFinal .&. tb.isDone
      , reset = resetRegAction >> tb.reset
      , failed = tb.failed
      , displayFailPure = when (tb.failed) (display_ (map pack vals) " ") >> (tb.displayFailPure)
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