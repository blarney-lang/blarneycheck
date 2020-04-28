{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}

module Check.PureProp where

import Blarney
import Check.Generator
import Check.Property
import Check.TestBench


propertyToPureTB :: Property -> Module PureTestBench
propertyToPureTB (name, prop) = do
  tb <- propToPureTB prop False
  return tb { displayFail = display_ name >> tb.displayFail }

combinePureProps :: [Property] -> Module PureTestBench
combinePureProps pureProps = do
  (ptb, _) <- combinePPs pureProps 0 1
  return ptb
  where
    combinePPs [] _ _ = error "No Assert Props given" -- Alternatively: propertyToPureTB ("True", Assert 1)
    combinePPs [prop] failSig doneSig = do
      tb <- propertyToPureTB prop
      return (PureTestBench { increment = when (tb.isDone.inv .|. doneSig) (tb.increment)
                            , isDone = tb.isDone .&. doneSig
                            , failed = tb.failed
                            , displayFail = when (tb.failed .|. failSig.inv) (tb.displayFail) 
                            }, failSig .|. tb.failed)
    combinePPs (prop:props) failSig doneSig = do
      tb <- propertyToPureTB prop
      (tbOthers, anyFail) <- combinePPs props (failSig .|. tb.failed) (tb.isDone .&. doneSig)
      return (PureTestBench { increment = do
                                when (tb.isDone.inv .|. tbOthers.isDone) (tb.increment)
                                tbOthers.increment
                            , isDone = tb.isDone .&. tbOthers.isDone
                            , failed = tb.failed .|. tbOthers.failed
                            , displayFail = do
                                when (tb.failed .|. anyFail.inv) do
                                  tb.displayFail
                                  when (tbOthers.failed .|. anyFail.inv) do display_ ", "
                                tbOthers.displayFail
                            }, anyFail)

propToPureTB :: Prop -> Bool -> Module(PureTestBench)
propToPureTB (WhenRecipe _ _) _ = error "WhenRecipe in Pure properties"
propToPureTB (WhenAction _ _) _ = error "WhenAction in Pure properties"
propToPureTB (Assert result) _ =
  return PureTestBench {
    increment = noAction
    , isDone = 1
    , failed = inv result
    , displayFail = noAction -- Test benches above have displayed name and arguments
  }
propToPureTB (Forall (f :: a -> Prop)) inList = do
  gen :: Reg a <- makeReg initial
  tb <- propToPureTB (f $ gen.val) inList
  let amFinal = gen.val.isFinal
  let nextVal = amFinal ? (initial, gen.val.next)
  let incrementAction = do { do
    gen <== nextVal
    when amFinal $ tb.increment
  }
  return PureTestBench { 
      increment = incrementAction
    , isDone = amFinal .&. tb.isDone
    , failed = tb.failed
    , displayFail = display_ " 0x" (pack $ gen.val) >> (tb.displayFail)
  }
propToPureTB (ForallList 0 f) _ = do
  tb <- propToPureTB (f []) False
  return tb {displayFail = display_ "]" >> tb.displayFail}
propToPureTB (ForallList maxLength f) inList = do
  tb <- propToPureTB (Forall $ \x -> ForallList (maxLength - 1) $ \xs -> f (x:xs)) True
  let delimiter = if inList then "," else " ["
  return tb {displayFail = display_ delimiter >> tb.displayFail}

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
                         , displayFail = tb.displayFail >> tbOthers.displayFail
                         }
-}