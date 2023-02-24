{-# language MagicHash, UnboxedTuples #-}

module Control.Prim.Lifted (Prim#, (>>=), (>>), pure, unsafePerformPrim#) where

import Control.Prim (Prim#)
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