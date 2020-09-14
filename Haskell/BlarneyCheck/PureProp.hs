{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}

module BlarneyCheck.PureProp where

import Blarney
import BlarneyCheck.Generator
import BlarneyCheck.Property
import BlarneyCheck.TestBench


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
      return (PureTestBench { increment = when (tb.isDone.inv .|. doneSig) do increment tb
                            , isDone = tb.isDone .&. doneSig
                            , failed = tb.failed
                            , displayFail = when (tb.failed .|. failSig.inv) do displayFail tb 
                            }, failSig .|. tb.failed)
    combinePPs (prop:props) failSig doneSig = do
      tb <- propertyToPureTB prop
      (tbOthers, anyFail) <- combinePPs props (failSig .|. tb.failed) (tb.isDone .&. doneSig)
      return (PureTestBench { increment = do
                                when (tb.isDone.inv .|. tbOthers.isDone) do increment tb
                                increment tbOthers
                            , isDone = tb.isDone .&. tbOthers.isDone
                            , failed = tb.failed .|. tbOthers.failed
                            , displayFail = do
                                when (tb.failed .|. anyFail.inv) do
                                  displayFail tb
                                  when (tbOthers.failed .|. anyFail.inv) do display_ ", "
                                displayFail tbOthers
                            }, anyFail)

propToPureTB :: Prop -> Bool -> Module(PureTestBench)
propToPureTB (WhenRecipe _ _) _ = error "WhenRecipe in Pure properties"
propToPureTB (WhenAction _ _) _ = error "WhenAction in Pure properties"
propToPureTB (Assert' result disp) _ = do
  ptb <- propToPureTB (Assert result) False
  return ptb { displayFail = display_ " {" >> disp >> display_ "}" }
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
      nextVal = amFinal ? (initial, gen.val.next)
  let incrementAction = do
        gen <== nextVal
        when amFinal do increment tb
  
  return PureTestBench { 
      increment = incrementAction
    , isDone = amFinal .&. tb.isDone
    , failed = tb.failed
    , displayFail = display_ " 0x" (gen.val.pack) >> displayFail tb
  }
propToPureTB (ForallList 0 f) _ = do
  tb <- propToPureTB (f []) False
  return tb {displayFail = display_ "]" >> displayFail tb}
propToPureTB (ForallList maxLength f) inList = do
  tb <- propToPureTB (Forall $ \x -> ForallList (maxLength - 1) $ \xs -> f (x:xs)) True
  let delimiter = if inList then "," else " ["
  return tb { displayFail = display_ delimiter >> displayFail tb }
