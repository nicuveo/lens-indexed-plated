{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}

-- | An indexed version of @Plated@.
--
-- TODO
module Control.Lens.IndexedPlated where

import Control.Applicative
import Control.Lens
import Data.Monoid


--------------------------------------------------------------------------------
-- * indexed plated

class IndexedPlated i a where
  -- | 'IndexedTraversal' of the immediate children of this structure.
  indexedPlate :: i -> IndexedTraversal' i a a


--------------------------------------------------------------------------------
-- * children

-- | Given an 'IndexedPlated' container and its index, extract the immediate
-- descendants of the container and their indices.
ichildren :: IndexedPlated i a => i -> a -> [(i, a)]
ichildren p = itoListOf (indexedPlate p)
{-# INLINE ichildren #-}


--------------------------------------------------------------------------------
-- * rewrite

irewrite :: IndexedPlated i a => (i -> a -> Maybe a) -> i -> a -> a
irewrite = irewriteOf indexedPlate
{-# INLINE irewrite #-}

irewriteOf :: (i -> IndexedSetter i a b a b) -> (i -> b -> Maybe a) -> i -> a -> b
irewriteOf l f = go
  where
    go = itransformOf l (\i x -> maybe x (go i) (f i x))
{-# INLINE irewriteOf #-}

irewriteOn :: IndexedPlated i a => ASetter s t a a -> (i -> a -> Maybe a) -> i -> s -> t
irewriteOn b f i = over b $ irewrite f i
{-# INLINE irewriteOn #-}

irewriteOnOf :: ASetter s t a b -> (i -> IndexedSetter i a b a b) -> (i -> b -> Maybe a) -> i -> s -> t
irewriteOnOf b l f i = over b $ irewriteOf l f i
{-# INLINE irewriteOnOf #-}

irewriteM :: (Monad m, IndexedPlated i a) => (i -> a -> m (Maybe a)) -> i -> a -> m a
irewriteM = irewriteMOf indexedPlate
{-# INLINE irewriteM #-}

irewriteMOf :: Monad m => (i -> IndexedLensLike i (WrappedMonad m) a b a b) -> (i -> b -> m (Maybe a)) -> i -> a -> m b
irewriteMOf l f = go
  where
    go = itransformMOf l (\i x -> f i x >>= maybe (pure x) (go i))
{-# INLINE irewriteMOf #-}

irewriteMOn :: (Monad m, IndexedPlated i a) => LensLike (WrappedMonad m) s t a a -> (i -> a -> m (Maybe a)) -> i -> s -> m t
irewriteMOn b f i = mapMOf b $ irewriteM f i
{-# INLINE irewriteMOn #-}

irewriteMOnOf :: Monad m => LensLike (WrappedMonad m) s t a b -> (i -> IndexedLensLike i (WrappedMonad m) a b a b) -> (i -> b -> m (Maybe a)) -> i -> s -> m t
irewriteMOnOf b l f i = mapMOf b $ irewriteMOf l f i
{-# INLINE irewriteMOnOf #-}


--------------------------------------------------------------------------------
-- * universe

iuniverse :: forall i a. IndexedPlated i a => i -> a -> [(i, a)]
iuniverse = iuniverseOf indexedPlate
{-# INLINE iuniverse #-}

iuniverseOf :: (i -> IndexedGetting i (Endo [(i, a)]) a a) -> i -> a -> [(i, a)]
iuniverseOf l = \i x -> appEndo (iuniverseOf' l i x) []
{-# INLINE iuniverseOf #-}

iuniverseOf' :: (i -> IndexedGetting i (Endo [(i, a)]) a a) -> i -> a -> Endo [(i, a)]
iuniverseOf' l = go
  where
    go i a = Endo ((i, a) :) <> ifoldMapOf (l i) go a
{-# INLINE iuniverseOf' #-}

iuniverseOn ::  IndexedPlated i a => Getting (Endo [(i, a)]) s a -> i -> s -> [(i, a)]
iuniverseOn b = iuniverseOnOf b indexedPlate
{-# INLINE iuniverseOn #-}

iuniverseOnOf :: Getting (Endo [(i, a)]) s a -> (i -> IndexedGetting i (Endo [(i, a)]) a a) -> i -> s -> [(i, a)]
iuniverseOnOf b p i x = appEndo (foldMapOf b (iuniverseOf' p i) x) []
{-# INLINE iuniverseOnOf #-}


--------------------------------------------------------------------------------
-- * cosmos

icosmos :: IndexedPlated i a => i -> IndexedFold i a a
icosmos = icosmosOf indexedPlate
{-# INLINE icosmos #-}

icosmosOf :: (Applicative f, Contravariant f) => (i -> IndexedLensLike' i f a a) -> (i -> IndexedLensLike' i f a a)
icosmosOf d i f s = indexed f i s *> d i (icosmosOf d i f) s
{-# INLINE icosmosOf #-}

icosmosOn :: (Applicative f, Contravariant f, IndexedPlated i a) => LensLike' f s a -> (i -> LensLike' f s a)
icosmosOn d = icosmosOnOf d indexedPlate
{-# INLINE icosmosOn #-}

icosmosOnOf :: (Applicative f, Contravariant f) => LensLike' f s a -> (i -> IndexedLensLike' i f a a) -> (i -> LensLike' f s a)
icosmosOnOf d p i = d . (icosmosOf p i)
{-# INLINE icosmosOnOf #-}


--------------------------------------------------------------------------------
-- * transform

itransform :: IndexedPlated i a => (i -> a -> a) -> i -> a -> a
itransform = itransformOf indexedPlate
{-# INLINE itransform #-}

itransformOn :: IndexedPlated i a => ASetter s t a a -> (i -> a -> a) -> i -> s -> t
itransformOn b i = over b . itransform i
{-# INLINE itransformOn #-}

itransformOf :: (i -> IndexedSetter i a b a b) -> (i -> b -> b) -> i -> a -> b
itransformOf l f = go
  where
    go i = f i . iover (l i) go
{-# INLINE itransformOf #-}

itransformOnOf :: ASetter s t a b -> (i -> IndexedSetter i a b a b) -> (i -> b -> b) -> i -> s -> t
itransformOnOf b l f = over b . itransformOf l f
{-# INLINE itransformOnOf #-}

itransformM :: (Monad m, IndexedPlated i a) => (i -> a -> m a) -> i -> a -> m a
itransformM = itransformMOf (\i -> indexedPlate i)
{-# INLINE itransformM #-}

itransformMOn :: (Monad m, IndexedPlated i a) => LensLike (WrappedMonad m) s t a a -> (i -> a -> m a) -> i -> s -> m t
itransformMOn b f = mapMOf b . itransformM f
{-# INLINE itransformMOn #-}

itransformMOf :: Monad m => (i -> IndexedLensLike i (WrappedMonad m) a b a b) -> (i -> b -> m b) -> i -> a -> m b
itransformMOf l f = go
  where
    go i t = imapMOf (l i) go t >>= f i
{-# INLINE itransformMOf #-}

itransformMOnOf :: Monad m => LensLike (WrappedMonad m) s t a b -> (i -> IndexedLensLike i (WrappedMonad m) a b a b) -> (i -> b -> m b) -> i -> s -> m t
itransformMOnOf b l f = mapMOf b . itransformMOf l f
{-# INLINE itransformMOnOf #-}


--------------------------------------------------------------------------------
-- * paramorphisms

ipara :: IndexedPlated i a => (i -> a -> [r] -> r) -> i -> a -> r
ipara = iparaOf indexedPlate
{-# INLINE ipara #-}

iparaOf :: (i -> IndexedGetting i (Endo [(i, a)]) a a) -> (i -> a -> [r] -> r) -> i -> a -> r
iparaOf l f = go
  where
    go i a = f i a [go j b | (j, b) <- itoListOf (l i) a]
{-# INLINE iparaOf #-}
