{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}

module Check.Property where

import Blarney
import Blarney.Recipe
import Check.Generator


type Property = (String, Prop)

data Prop where
  Assert :: Bit 1 -> Prop
  Assert' :: Bit 1 -> Action () -> Prop
  Forall :: Generator a => (a -> Prop) -> Prop
  ForallList :: Generator a => Int -> ([a] -> Prop) -> Prop
  WhenAction :: (Bit 1) -> Action() -> Prop
  WhenRecipe :: (Bit 1) -> Recipe -> Prop
