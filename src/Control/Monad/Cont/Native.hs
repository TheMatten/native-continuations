{-# language MagicHash, UnboxedTuples #-}
{-# options -Wno-redundant-constraints #-}

-- TODO: code examples

-- | Interface for native delimited continuations.
--
-- 'Cont' provides interface in style of @Control.Monad.Trans.Cont.Cont@, while 'ContST'
-- provides interface with tagged delimited continuations, scoping of which is tracked by
-- additional type parameter.
module Control.Monad.Cont.Native
  ( -- * 'Cont' Monad
    Cont, reset, shift, runCont
  , -- * 'ContST' Monad 
    ContST, ContTag, resetST, shiftST, runContST
  , Delimiter, type (-:), (:!>), 
  ) where

import Prelude

import Control.Prim (Prim#)
import Control.Prim.Lifted qualified as Lifted
import Control.Prim.Unlifted qualified as Unlifted
import Data.Kind (Constraint, Type)
import GHC.Exts (ZeroBitType, RealWorld, PromptTag#, newPromptTag#, prompt#, control0#)
import GHC.TypeError (TypeError, ErrorMessage (..))

------------------------------------------------------------------------------------------
-- | Monad with support for delimited continuations. Parameter @r@ tracks return type at
-- closest delimiter enclosing current expression, which may be either 'reset' or
-- 'runCont' at top level.
newtype Cont r a = UnsafeCont# (Prim# RealWorld a)

-- | Box holding 'PromptTag#' used by 'Cont'. Needed because top bindings have to be
-- lifted. There's only one instance of it, 'unsafeContTagBox'.
data TagBox = UnsafeTagBox# (forall a . PromptTag# a)

-- | Global instance of 'PromptTag#' used by 'Cont'. It's important to not get this
-- inlined, because 'newPromptTag#' is stateful.
unsafeContTagBox :: TagBox
unsafeContTagBox = UnsafeTagBox# do Unlifted.unsafePerformPrim# newPromptTag#
{-# noinline unsafeContTagBox #-}

unsafeRunCont# :: Cont r a -> Prim# RealWorld a
unsafeRunCont# (UnsafeCont# ca) = ca
{-# inline unsafeRunCont# #-}

instance Functor (Cont r) where
  fmap f (UnsafeCont# ca) = UnsafeCont# Lifted.do a <- ca; Lifted.pure (f a)
  {-# inline fmap #-}

instance Applicative (Cont r) where
  pure a = UnsafeCont# $ Lifted.pure a
  {-# inline pure #-}
  liftA2 f (UnsafeCont# ca) (UnsafeCont# cb) = UnsafeCont# Lifted.do
    a <- ca
    b <- cb
    Lifted.pure $ f a b
  {-# inline liftA2 #-}

instance Monad (Cont r) where
  UnsafeCont# ca >>= f = UnsafeCont# Lifted.do a <- ca; unsafeRunCont# $ f a
  {-# inline (>>=) #-}

-- | Delimits any 'shift' appearing directly under it's scope.
reset :: Cont a a -> Cont r a
reset (UnsafeCont# ca) =
  UnsafeCont# let !(UnsafeTagBox# t) = unsafeContTagBox in prompt# t ca

-- | Captures continuation up to nearest 'reset'/'runCont' scope. That is, argument of
-- this function takes over computation and receives rest of current scope as function
-- ("continuation"), which when called, will resume computation in current scope with
-- call of 'shift' "replaced" by it's argument.
shift :: ((Cont a b -> Cont a a) -> Cont a a) -> Cont a b
shift handler = UnsafeCont# do
  let !(UnsafeTagBox# t) = unsafeContTagBox
  control0# t \kba -> prompt# t $ unsafeRunCont# $
    -- 'prompt#' under 'handler', while not affecting safety, makes it impossible to
    -- "escape" handler - see formal definitions in
    -- https://okmij.org/ftp/continuations/impromptu-shift-tr.pdf
    handler \(UnsafeCont# cb) -> UnsafeCont# $ prompt# t $ kba cb

-- | Runs computation in 'Cont' monad.
runCont :: Cont a a -> a
runCont ca = Lifted.unsafePerformPrim# $ unsafeRunCont# $ reset ca
{-# inline runCont #-}

------------------------------------------------------------------------------------------
infixr 6 -:, :!>

data Delimiter = ZeroBitType :!> Type

-- | @s:!> a@ denotes delimited scope @s@ with result type @a@. New scopes can be created
-- with 'resetST'.
type s :!> a = s ':!> a

-- | Monad with support for tagged delimited continuations. That is, compared to 'Cont',
-- instead of closest result type scoped by 'reset', it's parameter tracks stack of scopes
-- paired with their result types. 'resetST' then provides 'ContTag' of it's scope, which
-- can be passed to 'shiftST' to receive continuation up to this scope.
type ContST :: [Delimiter] -> Type -> Type
newtype ContST ds a = UnsafeContST# (Prim# RealWorld a)

-- | Tag of scope @s@ with result type @a@ created by 'resetST'. Can be passed to
-- 'shiftST' to receive continuation up to this scope.
data ContTag s a = UnsafeContTag# (PromptTag# a)

unsafeRunContST# :: ContST ds a -> Prim# RealWorld a
unsafeRunContST# (UnsafeContST# ca) = ca
{-# inline unsafeRunContST# #-}

instance Functor (ContST ds) where
  fmap f (UnsafeContST# ca) = UnsafeContST# Lifted.do a <- ca; Lifted.pure (f a)
  {-# inline fmap #-}

instance Applicative (ContST ds) where
  pure a = UnsafeContST# $ Lifted.pure a
  {-# inline pure #-}
  liftA2 f (UnsafeContST# ca) (UnsafeContST# cb) = UnsafeContST# Lifted.do
    a <- ca
    b <- cb
    Lifted.pure $ f a b
  {-# inline liftA2 #-}

instance Monad (ContST ds) where
  UnsafeContST# ca >>= f = UnsafeContST# Lifted.do a <- ca; unsafeRunContST# $ f a
  {-# inline (>>=) #-}

-- | Delimits any 'shiftST' called with 'ContTag' of this scope.
resetST :: (forall s . ContTag s a -> ContST (s:!> a ': ds) a) -> ContST ds a
resetST f = UnsafeContST# Unlifted.do
  t <- newPromptTag#
  prompt# t $ unsafeRunContST# $ f $ UnsafeContTag# t
{-# inline resetST #-}

-- | Captures continuation up to scope of supplied 'ContTag'. Constraint on this function
-- makes sure that scope @s@ appears in stack of scopes enclosing current expression.
shiftST :: ds -:s:!> a
        => ContTag s a -> ((ContST ds b -> ContST ds a) -> ContST ds a) -> ContST ds b
shiftST (UnsafeContTag# t) handler = UnsafeContST# $
  control0# t \kba -> prompt# t $ unsafeRunContST# $
    -- See note in 'shift' about 'prompt#'.
    handler \(UnsafeContST# cb) -> UnsafeContST# $ prompt# t $ kba cb
{-# inline shiftST #-}

-- | Run computation without unenclosed 'shiftST' calls in 'ContST' monad.
runContST :: ContST '[] a -> a
runContST (UnsafeContST# ca) = Lifted.unsafePerformPrim# ca
{-# inline runContST #-}

-- | @ds -:s:!> a@ means that scope @s@ appears in stack of 'resetST' scopes with result
-- type @a@.
type (-:) :: [Delimiter] -> Delimiter -> Constraint
type family ds-:d where
  (d ':  _)-:d = ()
  (_ ': ds)-:d = ds-:d
  -- Not probable, but just in case.
  '[]      -:d = TypeError (NotDelimitedBy d)

type NotDelimitedBy d =
  'Text "Operation is not delimited by context '" ':<>: 'ShowType d ':<>: 'Text "'"
