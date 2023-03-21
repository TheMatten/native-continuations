{-# language MagicHash, UnboxedTuples #-}

module Control.Prim.Lifted
  ( Prim#, (>>=), (>>), pure, unsafePerformPrim#
  , Prim, unsafePerformPrim
  ) where

import Prelude (($))
import Prelude qualified as Prelude
import Control.Monad.Primitive (PrimMonad (..), PrimBase (..))
import Control.Prim (Prim#, unsafeCoercePrimState#)
import GHC.Exts (TYPE, RealWorld, runRW#)

------------------------------------------------------------------------------------------
(>>=) :: forall s a r (b :: TYPE r) . Prim# s a -> (a -> Prim# s b) -> Prim# s b
sa >>= f = \s0 -> case sa s0 of (# s1, a #) -> f a s1
{-# inline (>>=) #-}

(>>) :: forall s a r (b :: TYPE r) . Prim# s a -> Prim# s b -> Prim# s b
sa >> sb = \s0 -> case sa s0 of (# s1, _ #) -> sb s1
{-# inline (>>) #-}

pure :: forall s a . a -> Prim# s a
pure a = (# , a #)
{-# inline pure #-}

unsafePerformPrim# :: Prim# RealWorld a -> a
unsafePerformPrim# pa = case runRW# pa of (# _, a #) -> a
{-# inline unsafePerformPrim# #-}

newtype Prim a = Prim (Prim# () a)

primToPrim# :: Prim a -> Prim# () a
primToPrim# (Prim pa) = pa
{-# inline primToPrim# #-}

unsafePerformPrim :: Prim a -> a
unsafePerformPrim (Prim ma) = unsafePerformPrim# $ unsafeCoercePrimState# ma
{-# inline unsafePerformPrim #-}

instance Prelude.Functor Prim where
  fmap f (Prim pa) = Prim $ pa >>= \a -> pure $ f a
  {-# inline fmap #-}

instance Prelude.Applicative Prim where
  pure a = Prim $ pure a
  {-# inline pure #-}
  Prim pf <*> Prim pa = Prim $ pf >>= \f -> pa >>= \a -> pure $ f a
  {-# inline (<*>) #-}

instance Prelude.Monad Prim where
  Prim pa >>= f = Prim $ pa >>= \a -> primToPrim# $ f a
  {-# inline (>>=) #-}

instance PrimMonad Prim where
  type PrimState Prim = ()
  primitive = Prim
  {-# inline primitive #-}

instance PrimBase Prim where
  internal = primToPrim#
  {-# inline internal #-}