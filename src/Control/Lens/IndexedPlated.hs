{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}

-- | An indexed version of @Plated@.
--
-- This module provides a similar API to lens' @Plated@, with the following key
-- differences:
--
--   * all corresponding functions from @Plated@ have a @i@ prefix, similarly to
--     what lens does (see @over@ and @iover@ for instance);
--   * all @Getter@, @Setter@ and @LensLike@ are replaced by their @Indexed@
--     equivalents, and all take an additional index parameter
module Control.Lens.IndexedPlated
  ( -- * Indexed plated
    IndexedPlated (..)
    -- * Children
  , ichildren
    -- * Rewrite
  , irewrite
  , irewriteOf
  , irewriteOn
  , irewriteOnOf
  , irewriteM
  , irewriteMOf
  , irewriteMOn
  , irewriteMOnOf
    -- * Universe
  , iuniverse
  , iuniverseOf
  , iuniverseOn
  , iuniverseOnOf
    -- * Cosmos
  , icosmos
  , icosmosOf
  , icosmosOn
  , icosmosOnOf
    -- * Transform
  , itransform
  , itransformOf
  , itransformOn
  , itransformOnOf
  , itransformM
  , itransformMOf
  , itransformMOn
  , itransformMOnOf
    -- * Paramorphisms
  , ipara
  , iparaOf
  ) where

import Control.Applicative
import Control.Lens
import Data.Monoid


--------------------------------------------------------------------------------
-- Indexed plated

class IndexedPlated i a where
  -- | 'IndexedTraversal' of the immediate children of this structure.
  iplate :: i -> IndexedTraversal' i a a


--------------------------------------------------------------------------------
-- Children

-- | Given an 'IndexedPlated' container and its index, extract the immediate
-- descendants of the container and their indices.
ichildren :: IndexedPlated i a => i -> a -> [(i, a)]
ichildren p = itoListOf (iplate p)
{-# INLINE ichildren #-}


--------------------------------------------------------------------------------
-- Rewrite

-- | Rewrite a container by applying a rule everywhere possible. If the rule
-- returns @Nothing@, the value remains unchanged, but while it returns a new
-- value the transformation will be recursively applied. This guarantees that
-- this function is applied everywhere possible, and that the result does not
-- contain any eligible value.
irewrite :: IndexedPlated i a => (i -> a -> Maybe a) -> i -> a -> a
irewrite = irewriteOf iplate
{-# INLINE irewrite #-}

-- | Rewrite a container by applying a rule everywhere possible, using the
-- provided lens to locate immediate children. If the rule returns @Nothing@,
-- the value remains unchanged, but while it returns a new value the
-- transformation will be recursively applied. This guarantees that this
-- function is applied everywhere possible, and that the result does not contain
-- any eligible value.
irewriteOf :: (i -> IndexedSetter i a b a b) -> (i -> b -> Maybe a) -> i -> a -> b
irewriteOf l f = go
  where
    go = itransformOf l (\i x -> maybe x (go i) (f i x))
{-# INLINE irewriteOf #-}

-- | Similar to 'irewrite', but performed recursively over part of a larger
-- structure.
irewriteOn :: IndexedPlated i a => ASetter s t a a -> (i -> a -> Maybe a) -> i -> s -> t
irewriteOn b f i = over b $ irewrite f i
{-# INLINE irewriteOn #-}

-- | Similar to 'irewriteOf', but performed recursively over part of a larger
-- structure.
irewriteOnOf :: ASetter s t a b -> (i -> IndexedSetter i a b a b) -> (i -> b -> Maybe a) -> i -> s -> t
irewriteOnOf b l f i = over b $ irewriteOf l f i
{-# INLINE irewriteOnOf #-}

-- | Similar to 'irewrite', but using a monadic rule.
irewriteM :: (Monad m, IndexedPlated i a) => (i -> a -> m (Maybe a)) -> i -> a -> m a
irewriteM = irewriteMOf iplate
{-# INLINE irewriteM #-}

-- | Similar to 'irewriteOf', but using a monadic rule.
irewriteMOf :: Monad m => (i -> IndexedLensLike i (WrappedMonad m) a b a b) -> (i -> b -> m (Maybe a)) -> i -> a -> m b
irewriteMOf l f = go
  where
    go = itransformMOf l (\i x -> f i x >>= maybe (pure x) (go i))
{-# INLINE irewriteMOf #-}

-- | Similar to 'irewriteOn', but using a monadic rule.
irewriteMOn :: (Monad m, IndexedPlated i a) => LensLike (WrappedMonad m) s t a a -> (i -> a -> m (Maybe a)) -> i -> s -> m t
irewriteMOn b f i = mapMOf b $ irewriteM f i
{-# INLINE irewriteMOn #-}

-- | Similar to 'irewriteOnOf', but using a monadic rule.
irewriteMOnOf :: Monad m => LensLike (WrappedMonad m) s t a b -> (i -> IndexedLensLike i (WrappedMonad m) a b a b) -> (i -> b -> m (Maybe a)) -> i -> s -> m t
irewriteMOnOf b l f i = mapMOf b $ irewriteMOf l f i
{-# INLINE irewriteMOnOf #-}


--------------------------------------------------------------------------------
-- Universe

-- | Retrieve all of the transitive descendants (and their indices) of an
-- 'IndexedPlated' container, including itself.
iuniverse :: forall i a. IndexedPlated i a => i -> a -> [(i, a)]
iuniverse = iuniverseOf iplate
{-# INLINE iuniverse #-}

-- | Retrieve all of the transitive descendants (and their indices) of a
-- container, including itself, using the provided lens to locate immediate
-- children.
iuniverseOf :: (i -> IndexedGetting i (Endo [(i, a)]) a a) -> i -> a -> [(i, a)]
iuniverseOf l = \i x -> appEndo (iuniverseOf' l i x) []
{-# INLINE iuniverseOf #-}

iuniverseOf' :: (i -> IndexedGetting i (Endo [(i, a)]) a a) -> i -> a -> Endo [(i, a)]
iuniverseOf' l = go
  where
    go i a = Endo ((i, a) :) <> ifoldMapOf (l i) go a
{-# INLINE iuniverseOf' #-}

-- | Similar to 'iuniverse', but performed recursively over part of a larger
-- structure.
iuniverseOn ::  IndexedPlated i a => Getting (Endo [(i, a)]) s a -> i -> s -> [(i, a)]
iuniverseOn b = iuniverseOnOf b iplate
{-# INLINE iuniverseOn #-}

-- | Similar to 'iuniverseOf', but performed recursively over part of a larger
-- structure.
iuniverseOnOf :: Getting (Endo [(i, a)]) s a -> (i -> IndexedGetting i (Endo [(i, a)]) a a) -> i -> s -> [(i, a)]
iuniverseOnOf b p i x = appEndo (foldMapOf b (iuniverseOf' p i) x) []
{-# INLINE iuniverseOnOf #-}


--------------------------------------------------------------------------------
-- Cosmos

-- | Fold over all transitive descendants (and their indices) of an
-- 'IndexedPlated' container, including itself.
icosmos :: IndexedPlated i a => i -> IndexedFold i a a
icosmos = icosmosOf iplate
{-# INLINE icosmos #-}

-- | Fold over all transitive descendants (and their indices) of a container,
-- including itself, using the provided lens to locate immediate children.
icosmosOf :: (Applicative f, Contravariant f) => (i -> IndexedLensLike' i f a a) -> (i -> IndexedLensLike' i f a a)
icosmosOf d i f s = indexed f i s *> d i (icosmosOf d i f) s
{-# INLINE icosmosOf #-}

-- | Similar to 'icosmos', but performed recursively over part of a larger
-- structure.
icosmosOn :: (Applicative f, Contravariant f, IndexedPlated i a) => LensLike' f s a -> (i -> LensLike' f s a)
icosmosOn d = icosmosOnOf d iplate
{-# INLINE icosmosOn #-}

-- | Similar to 'icosmosOf', but performed recursively over part of a larger
-- structure.
icosmosOnOf :: (Applicative f, Contravariant f) => LensLike' f s a -> (i -> IndexedLensLike' i f a a) -> (i -> LensLike' f s a)
icosmosOnOf d p i = d . icosmosOf p i
{-# INLINE icosmosOnOf #-}


--------------------------------------------------------------------------------
-- Transform

-- | Recursively transform every element in the structure, in a bottom-up
-- manner.
itransform :: IndexedPlated i a => (i -> a -> a) -> i -> a -> a
itransform = itransformOf iplate
{-# INLINE itransform #-}

-- | Recursively transform every element in the structure, in a bottom-up
-- manner, using the provided lens to locate immediate children.
itransformOf :: (i -> IndexedSetter i a b a b) -> (i -> b -> b) -> i -> a -> b
itransformOf l f = go
  where
    go i = f i . iover (l i) go
{-# INLINE itransformOf #-}

-- | Similar to 'itransform', but performed recursively over part of a larger
-- structure.
itransformOn :: IndexedPlated i a => ASetter s t a a -> (i -> a -> a) -> i -> s -> t
itransformOn b i = over b . itransform i
{-# INLINE itransformOn #-}

-- | Similar to 'itransformOf', but performed recursively over part of a larger
-- structure.
itransformOnOf :: ASetter s t a b -> (i -> IndexedSetter i a b a b) -> (i -> b -> b) -> i -> s -> t
itransformOnOf b l f = over b . itransformOf l f
{-# INLINE itransformOnOf #-}

-- | Similar to 'itransform', but using a monadic rule.
itransformM :: (Monad m, IndexedPlated i a) => (i -> a -> m a) -> i -> a -> m a
itransformM = itransformMOf iplate
{-# INLINE itransformM #-}

-- | Similar to 'itransformOn', but using a monadic rule.
itransformMOn :: (Monad m, IndexedPlated i a) => LensLike (WrappedMonad m) s t a a -> (i -> a -> m a) -> i -> s -> m t
itransformMOn b f = mapMOf b . itransformM f
{-# INLINE itransformMOn #-}

-- | Similar to 'itransformOf', but using a monadic rule.
itransformMOf :: Monad m => (i -> IndexedLensLike i (WrappedMonad m) a b a b) -> (i -> b -> m b) -> i -> a -> m b
itransformMOf l f = go
  where
    go i t = imapMOf (l i) go t >>= f i
{-# INLINE itransformMOf #-}

-- | Similar to 'itransformOnOf', but using a monadic rule.
itransformMOnOf :: Monad m => LensLike (WrappedMonad m) s t a b -> (i -> IndexedLensLike i (WrappedMonad m) a b a b) -> (i -> b -> m b) -> i -> s -> m t
itransformMOnOf b l f = mapMOf b . itransformMOf l f
{-# INLINE itransformMOnOf #-}


--------------------------------------------------------------------------------
-- Paramorphisms

-- | Perform a fold-like computation on each value within a container.
ipara :: IndexedPlated i a => (i -> a -> [r] -> r) -> i -> a -> r
ipara = iparaOf iplate
{-# INLINE ipara #-}

-- | Perform a fold-like computation on each value within a container, using the
-- provided lens to locate immediate children.
iparaOf :: (i -> IndexedGetting i (Endo [(i, a)]) a a) -> (i -> a -> [r] -> r) -> i -> a -> r
iparaOf l f = go
  where
    go i a = f i a [go j b | (j, b) <- itoListOf (l i) a]
{-# INLINE iparaOf #-}
