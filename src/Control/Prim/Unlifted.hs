{-# language MagicHash, UnboxedTuples #-}

module Control.Prim.Unlifted (Prim#, (>>=), (>>), pure, unsafePerformPrim#) where

import Control.Prim (Prim#)
import GHC.Exts (TYPE, UnliftedType, RealWorld, runRW#)

------------------------------------------------------------------------------------------
(>>=) :: forall s (a :: UnliftedType) r (b :: TYPE r)
       . Prim# s a -> (a -> Prim# s b) -> Prim# s b
sa >>= f = \s0 -> case sa s0 of (# s1, a #) -> f a s1
{-# inline (>>=) #-}

(>>) :: forall s (a :: UnliftedType) r (b :: TYPE r)
      . Prim# s a -> Prim# s b -> Prim# s b
sa >> sb = \s0 -> case sa s0 of (# s1, _ #) -> sb s1
{-# inline (>>) #-}

pure :: forall s (a :: UnliftedType) . a -> Prim# s a
pure a = (# , a #)
{-# inline pure #-}

unsafePerformPrim# :: forall (a :: UnliftedType) . Prim# RealWorld a -> a
unsafePerformPrim# pa = case runRW# pa of (# _, a #) -> a
{-# inline unsafePerformPrim# #-}