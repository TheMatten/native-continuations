{-# language MagicHash #-}
{-# options -Wno-redundant-constraints #-}

-- TODO: code examples

-- | Interface for native delimited continuations.
--
-- 'Cont'/'ContT' provides interface in style of @Control.Monad.Trans.Cont.Cont@, while
-- 'ContST'/'ContSTT' provides interface with tagged delimited continuations, scoping of
-- which is tracked by additional type parameter.
module Control.Monad.Cont.Native
  ( -- * 'Cont' Monad
    ContT, Cont, reset, shift, runContT, runCont
  , -- * 'ContST' Monad 
    ContSTT, ContST, ContTag, resetST, shiftST, runContSTT, runContST
    -- ** 'ContST' delimiters
  , Delimiter, type (-:), (:!>), 
  ) where

import Prelude (Applicative (..), Functor (..), Monad (..), IO, (.), ($))
import Control.Monad.Primitive (PrimBase (..), PrimMonad (..))
import Control.Monad.ST.Strict (ST)
import Control.Monad.Trans.Class (MonadTrans (..))
import Control.Prim (PromptTag#, newPromptTag#, prompt#, control0#)
import Control.Prim.Lifted (Prim, unsafePerformPrim)
import Control.Prim.Unlifted qualified as Unlifted
import Data.Kind (Constraint, Type)
import GHC.Exts (ZeroBitType)
import GHC.TypeError (TypeError, ErrorMessage (..))

------------------------------------------------------------------------------------------
-- | Monad with support for delimited continuations. Parameter @r@ tracks return type at
-- closest delimiter enclosing current expression, which may be either 'reset' or
-- 'runCont'/'runContT' at top level. Underlying @m@ must be 'IO' or strict 'ST', or
-- 'Prim' used by pure 'Cont'.
type    ContT :: Type -> (Type -> Type) -> Type -> Type
newtype ContT r m a = UnsafeContT (m a)
  deriving newtype (Applicative, Functor, Monad)

instance MonadTrans (ContT r) where
  lift = UnsafeContT
  {-# inline lift #-}

-- | Box holding 'PromptTag#' used by 'Cont'. Needed because top bindings have to be
-- lifted. There's only one instance of it, 'unsafeContTagBox'.
data TagBox = UnsafeTagBox# (forall a . PromptTag# a)

-- | Global instance of 'PromptTag#' used by 'Cont'. It's important to not get this
-- inlined, because 'newPromptTag#' is stateful.
unsafeContTagBox :: TagBox
unsafeContTagBox = UnsafeTagBox# do Unlifted.unsafePerformPrim# newPromptTag#
{-# noinline unsafeContTagBox #-}

-- | Unwraps 'ContT' without making sure that top-level handler is installed.
unsafeRunContT :: ContT r m a -> m a
unsafeRunContT (UnsafeContT ma) = ma
{-# inline unsafeRunContT #-}

-- | Delimits any 'shift' appearing directly under it's scope.
reset :: StrictPrim m => ContT a m a -> ContT r m a
reset (UnsafeContT ma) = UnsafeContT $ primitive do
  let !(UnsafeTagBox# t) = unsafeContTagBox
  prompt# t $ internal ma
{-# inline reset #-}

-- | Captures continuation up to the nearest 'reset'/'runCont' scope. That is, argument
-- of this function takes over computation and receives rest of current scope as function
-- ("continuation"), which when called, will resume computation in current scope with
-- call of 'shift' "replaced" by it's argument.
shift :: StrictPrim m => ((ContT a m b -> ContT a m a) -> ContT a m a) -> ContT a m b
shift handler = UnsafeContT $ primitive do
  let !(UnsafeTagBox# t) = unsafeContTagBox
  control0# t \kba ->
    prompt# t $ internal $ unsafeRunContT $
      -- 'prompt#' under 'handler', while not affecting safety, makes it impossible to
      -- "escape" handler - see formal definitions in
      -- https://okmij.org/ftp/continuations/impromptu-shift-tr.pdf
      handler \(UnsafeContT cb) -> UnsafeContT $
        primitive $ prompt# t $ kba $ internal cb
{-# inline shift #-}

-- | Runs 'ContT' in underlying @m@, with 'reset' at top level.
runContT :: StrictPrim m => ContT a m a -> m a
runContT = unsafeRunContT . reset
{-# inline runContT #-}

-- | Class of monads that implement 'PrimBase' and are strict in their state.
class PrimBase m => StrictPrim m
instance StrictPrim IO
instance StrictPrim (ST s)
instance StrictPrim Prim

-- | Pure version of 'ContT'.
type Cont a = ContT a Prim

-- | Runs computation in 'Cont' monad, with 'reset' at top level.
runCont :: Cont a a -> a
runCont = unsafePerformPrim . runContT
{-# inline runCont #-}

------------------------------------------------------------------------------------------
infixr 6 -:, :!>

data Delimiter = ZeroBitType :!> Type

-- | @s:!> a@ denotes delimited scope @s@ with result type @a@. New scopes can be created
-- with 'resetST'.
type s :!> a = s ':!> a

-- | Monad with support for tagged delimited continuations. That is, compared to 'ContT',
-- instead of closest result type scoped by 'reset', it's parameter tracks stack of scopes
-- paired with their result types. 'resetST' then provides 'ContTag' of it's scope, which
-- can be passed to 'shiftST' to receive continuation up to this scope. Underlying @m@
-- must be 'IO' or strict 'ST', or 'Prim' used by pure 'ContST'.
type ContSTT :: [Delimiter] -> (Type -> Type) -> Type -> Type
newtype ContSTT ds m a = UnsafeContSTT (m a)
  deriving newtype (Applicative, Functor, Monad)

instance MonadTrans (ContSTT ds) where
  lift = UnsafeContSTT
  {-# inline lift #-}

-- | Tag of scope @s@ with result type @a@ created by 'resetST'. Can be passed to
-- 'shiftST' to receive continuation up to this scope.
data ContTag s a = UnsafeContTag# (PromptTag# a)

unsafeRunContSTT :: ContSTT ds m a -> m a
unsafeRunContSTT (UnsafeContSTT ca) = ca
{-# inline unsafeRunContSTT #-}

-- | Delimits any 'shiftST' called with 'ContTag' of this scope.
resetST
  :: StrictPrim m
  => (forall s . ContTag s a -> ContSTT (s:!> a ': ds) m a) -> ContSTT ds m a
resetST f = UnsafeContSTT $ primitive Unlifted.do
  t <- newPromptTag#
  prompt# t $ internal $ unsafeRunContSTT $ f $ UnsafeContTag# t
{-# inline resetST #-}

-- | Captures continuation up to scope of supplied 'ContTag'. Constraint on this function
-- makes sure that scope @s@ appears in stack of scopes enclosing current expression.
shiftST
  :: (ds -:s:!> a, StrictPrim m)
  => ContTag s a
  -> ((ContSTT ds m b -> ContSTT ds m a) -> ContSTT ds m a) -> ContSTT ds m b
shiftST (UnsafeContTag# t) handler = UnsafeContSTT $ primitive $
  control0# t \kba -> prompt# t $ internal $ unsafeRunContSTT $
    -- See note in 'shift' about 'prompt#'.
    handler \(UnsafeContSTT cb) -> UnsafeContSTT $ primitive $
      prompt# t $ kba $ internal cb
{-# inline shiftST #-}

-- | Runs 'ContSTT' computation without unenclosed 'shiftST' calls in underlying @m@.
runContSTT :: ContSTT '[] m a -> m a
runContSTT = unsafeRunContSTT
{-# inline runContSTT #-}

-- | Pure version of 'ContSTT'.
type ContST ds = ContSTT ds Prim

-- | Runs computation in 'ContST' monad.
runContST :: ContST '[] a -> a
runContST = unsafePerformPrim . unsafeRunContSTT
{-# inline runContST #-}

-- | @ds -:s:!> a@ means that scope @s@ appears in stack of 'resetST' scopes with result
-- type @a@.
type (-:) :: [Delimiter] -> Delimiter -> Constraint
type family ds-:d where
  (d ':  _)-:d = ()
  (_ ': ds)-:d = ds-:d
  '[]      -:d = TypeError (NotDelimitedBy d)

type NotDelimitedBy d =
  'Text "Operation is not delimited by context '" ':<>: 'ShowType d ':<>: 'Text "'"
