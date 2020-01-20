-- Problem is that we cannot test aribtrary data structures (eg. that the user has defined)
-- Solution is to come up with a generator datatype which the user can specify for custom types and these can then be tested
-- Two possiblilities of stateful vs stateless generator.

-- Do we need to support user defined data?
-- Need some way to guarantee that 't' can be stored in a register(s?), how to do for lists?

{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE BlockArguments         #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE NoImplicitPrelude      #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE DeriveAnyClass         #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE GADTs #-}

module Check3.Generator where

import Blarney
import Blarney.Queue

type SizedBits a = (Bits a, KnownNat (SizeOf a))


data Next where
  NApply :: (Bit 1) -> (Action ()) -> Next
  NForall :: Generator a => (Bit 1) -> (a -> Next) -> Next

class Generator a where
    initial :: Int -> Module(a)
    reset :: a -> Action ()
    next :: a -> [Next]

instance (SizedBits a) => Generator (Reg a) where
    initial = \_ -> makeReg (unpack (constant 0))
    reset = \state -> state <== (unpack (constant 0))
    next = \state -> [NApply (pack (state.val) .!=. ones) (state <== (unpack $ (pack (state.val)) .+. 1))]

instance (Generator a) => Generator [a] where
    initial = \size -> mapM (\_ -> initial size) [0 .. size]
    reset = \state -> foldr (>>) noAction (map reset state)
    next = \state -> concat (map next state)

instance (SizedBits t) => Generator (Queue t) where
    initial = \_ -> makeQueue
    reset = \queue -> queue.reset
    next = \queue -> [NForall (queue.notFull) \(x :: Reg t) -> NApply (constant 0) $ (queue.enq) (x.val), NApply (queue.notEmpty) (queue.deq)]


{-
data GeneratorState t = GeneratorState 
    { peek :: t
    , isLast :: Bit 1
    , reset :: Action ()
    , consume :: Action ()
    } deriving (Generic, Interface)


bitGen :: Bits a => Module (GeneratorState a)
bitGen = do

    Generator {
    peek = constant 0,
    next = \prev -> prev .+. 1,
    isLast = \value -> value .==. ones
}
-}