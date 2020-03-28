{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}

module Check.PureProp where

import Blarney
import Check.Generator
import Check.Property
import Check.TestBench
import Check.Utils


propertyToPureTB :: Property -> Module PureTestBench
propertyToPureTB (name, prop) = do
  tb <- propToPureTB prop
  return PureTestBench { increment = tb.increment
                       , isDone = tb.isDone
                       , reset = tb.reset
                       , failed = tb.failed
                       , displayFailPure = when (tb.failed) (display_ "*** " name " ") >> (tb.displayFailPure)
                       }

combinePureProps :: [Property] -> Module PureTestBench
combinePureProps pureProps = do
  combinePPs pureProps
  where
    combinePPs [] = error "No Assert Props given" -- Alternatively: propertyToPureTB ("True", Assert 1)
    combinePPs [prop] = propertyToPureTB prop
    combinePPs (prop:props) = do
      tb <- propertyToPureTB prop
      tbOthers <- combinePPs props
      return PureTestBench { increment = tb.increment >> tbOthers.increment
                           , isDone = tb.isDone .&. tbOthers.isDone
                           , reset = tb.reset >> tbOthers.reset
                           , failed = tb.failed .|. tbOthers.failed
                           , displayFailPure = tb.displayFailPure >> tbOthers.displayFailPure
                           }

propToPureTB :: Prop -> Module(PureTestBench)
propToPureTB (WhenRecipe _ _) = error "WhenRecipe in Pure properties"
propToPureTB (WhenAction _ _) = error "WhenAction in Pure properties"
propToPureTB (Assert result) =
  return PureTestBench {
    increment = noAction
    , isDone = 1
    , reset = noAction
    , failed = inv result
    , displayFailPure = when (inv result) (display "failed! ***")
  }
propToPureTB (Forall f) = do
    gen <- makeReg initial
    tb <- propToPureTB (f $ gen.val)
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
propToPureTB (ForallList maxLength f) = do
    gens <- mapM makeReg (replicate maxLength initial)
    let vals = map val gens
    let resetRegAction = doActionList (map (\(r, nV) -> r <== nV) (zip gens (replicate maxLength initial)))
    let incRegAction = doActionList (map (\(r, nV) -> r <== nV) (zip gens (incrementGenList vals)))
    let amFinal = andList $ map isFinal vals
    tb <- propToPureTB (f vals)
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