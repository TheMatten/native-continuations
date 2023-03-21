{-# language MagicHash, UnboxedTuples #-}

module Control.Prim
  ( Prim#, PromptTag#, control0#, newPromptTag#, prompt#, unsafeCoercePrimState#
  ) where

import Data.Kind (Type)
import GHC.Exts (PromptTag#, State#, TYPE, unsafeCoerce#)
import GHC.Exts qualified as GHC

------------------------------------------------------------------------------------------
type Prim# :: forall r . Type -> TYPE r -> Type
type Prim# s a = State# s -> (# State# s, a #)

unsafeCoercePrimState# :: forall s t r (a :: TYPE r) . Prim# s a -> Prim# t a
unsafeCoercePrimState# = unsafeCoerce#
{-# inline unsafeCoercePrimState# #-}

control0#
  :: forall s a b . PromptTag# a -> ((Prim# s b -> Prim# s a) -> Prim# s a) -> Prim# s b
control0# = unsafeCoerce# GHC.control0#
{-# inline control0# #-}

prompt# :: forall s a . PromptTag# a -> Prim# s a -> Prim# s a
prompt# = unsafeCoerce# GHC.prompt#
{-# inline prompt# #-}

newPromptTag# :: forall s a . Prim# s (PromptTag# a)
newPromptTag# = unsafeCoercePrimState# GHC.newPromptTag#
{-# inline newPromptTag# #-}
