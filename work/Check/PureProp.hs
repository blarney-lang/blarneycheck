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
  tb <- propToPureTB prop False
  return tb { displayFailPure = display_ "*** " name >> (tb.displayFailPure) }

combinePureProps :: [Property] -> Module PureTestBench
combinePureProps pureProps = do
  combinePPs pureProps
  where
    combinePPs [] = error "No Assert Props given" -- Alternatively: propertyToPureTB ("True", Assert 1)
    combinePPs [prop] = do
      tb <- propertyToPureTB prop
      return tb { displayFailPure = when (tb.failed) (tb.displayFailPure) }
    combinePPs (prop:props) = do
      tb <- propertyToPureTB prop
      tbOthers <- combinePPs props
      return PureTestBench { increment = tb.increment >> tbOthers.increment
                           , isDone = tb.isDone .&. tbOthers.isDone
                           , failed = tb.failed .|. tbOthers.failed
                           , displayFailPure = when (tb.failed) (tb.displayFailPure) >> tbOthers.displayFailPure
                           }

propToPureTB :: Prop -> Bool -> Module(PureTestBench)
propToPureTB (WhenRecipe _ _) _ = error "WhenRecipe in Pure properties"
propToPureTB (WhenAction _ _) _ = error "WhenAction in Pure properties"
propToPureTB (Assert result) _ =
  return PureTestBench {
    increment = noAction
    , isDone = 1
    , failed = inv result
    , displayFailPure = display " failed! ***"
  }
propToPureTB (Forall (f :: a -> Prop)) inList = do
  gen :: Reg a <- makeReg initial
  tb <- propToPureTB (f $ gen.val) inList
  let amFinal = (gen.val :: a).isFinal
  let nextVal = amFinal ? (initial, gen.val.next)
  let incrementAction = do { do
    gen <== nextVal
    when amFinal $ tb.increment
  }
  return PureTestBench { 
      increment = incrementAction
    , isDone = amFinal .&. tb.isDone
    , failed = tb.failed
    , displayFailPure = display_ " " (pack $ gen.val) >> (tb.displayFailPure)
  }
propToPureTB (ForallList 0 f) _ = do
  tb <- propToPureTB (f []) False
  return tb {displayFailPure = display_ "]" >> tb.displayFailPure}
propToPureTB (ForallList maxLength f) inList = do
  tb <- propToPureTB (Forall $ \x -> ForallList (maxLength - 1) $ \xs -> f (x:xs)) True
  let delimiter = if inList then "," else " ["
  return tb {displayFailPure = display_ delimiter >> tb.displayFailPure}

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