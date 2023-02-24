{-# language MagicHash, UnboxedTuples #-}

module Control.Prim (Prim#) where

import Data.Kind (Type)
import GHC.Exts (State#, TYPE)

type Prim# :: forall r . Type -> TYPE r -> Type
type Prim# s a = State# s -> (# State# s, a #)
