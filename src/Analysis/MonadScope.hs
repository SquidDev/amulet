{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Analysis.MonadScope(MonadScope, getScope, putScope, localScope) where

import Control.Monad.Except

import Control.Monad.State.Lazy as Lazy(StateT, get, put, evalStateT)
import Control.Monad.State.Strict as Strict(StateT, get, put, evalStateT)
import Control.Monad.Writer.Lazy as Lazy(WriterT, mapWriterT)
import Control.Monad.Writer.Strict as Strict(WriterT, mapWriterT)

class Monad m => MonadScope s m | m -> s where
  -- | Return the scope from the internals of the monad
  getScope :: m s
  -- | Replace the scope inside the monad
  putScope :: s -> m ()
  -- | Run the function with a new scope
  localScope :: (s -> s) -> m a -> m a

instance Monad m => MonadScope s (Lazy.StateT s m) where
  getScope = Lazy.get
  putScope = Lazy.put
  localScope f m = Lazy.get >>= \s -> lift $ Lazy.evalStateT m (f s)

instance Monad m => MonadScope s (Strict.StateT s m) where
  getScope = Strict.get
  putScope = Strict.put
  localScope f m = Strict.get >>= \s -> lift $ Strict.evalStateT m (f s)

instance MonadScope s m => MonadScope s (ExceptT e m) where
  getScope = lift getScope
  putScope = lift . putScope
  localScope = mapExceptT . localScope

instance (Monoid w, MonadScope s m) => MonadScope s (Lazy.WriterT w m) where
  getScope = lift getScope
  putScope = lift . putScope
  localScope = Lazy.mapWriterT . localScope

instance (Monoid w, MonadScope s m) => MonadScope s (Strict.WriterT w m) where
  getScope = lift getScope
  putScope = lift . putScope
  localScope = Strict.mapWriterT . localScope
