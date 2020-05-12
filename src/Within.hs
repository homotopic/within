{-# LANGUAGE DeriveFoldable        #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE DeriveTraversable     #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Within (
  WithinT(..)
, Within(..)
, localDir
, localDirM
, asWithin
, within
, fromWithin
, blinkLocalDir
, blinkAndMap
, blinkAndMapM
, localDirAndMapM
) where

import           Control.Applicative
import           Control.Comonad
import           Control.Comonad.Env
import           Control.Monad
import           Control.Monad.Catch
import           Data.Functor.Identity
import           Data.Hashable
import           Data.Typeable
import           GHC.Generics
import           Path

-- | Transform the environment of an `EnvT` via some monadic operation. This didn't seem to
-- exist anywhere but it used for some internal operations.
localM :: Monad m => (e -> m e') -> EnvT e w a -> m (EnvT e' w a)
localM f (EnvT e wa) = f e >>= \e' -> return $ EnvT e' wa

-- | The Within Type, newtype wrapper around `EnvT` specialised to a `Path b Dir` environment.
newtype WithinT b w a = WithinT (EnvT (Path b Dir) w a)
  deriving (Functor, Foldable, Traversable)

type Within b a = WithinT b Identity a

instance Comonad w => Comonad (WithinT b w) where
  extract (WithinT w) = extract w
  duplicate (WithinT w) = WithinT (extend WithinT w)

instance Comonad w => ComonadEnv (Path b Dir) (WithinT b w) where
  ask (WithinT w) = ask w

instance ComonadTrans (WithinT b) where
  lower (WithinT w) = lower w

-- | Change the parent directory of a `Within` value.
localDir :: (Path b Dir -> Path c Dir) -> WithinT b w a -> WithinT c w a
localDir f (WithinT w) = WithinT (local f w)

-- | Change the parent directory of a `Within` value, monadic verison.
localDirM :: Monad m => (Path b Dir -> m (Path c Dir)) -> WithinT b w a -> m (WithinT c w a)
localDirM f (WithinT (EnvT e wa)) = f e >>= \e' -> return $ WithinT $ EnvT e' wa

-- | Treat a `Path` as if it lies within another directory and returns an Env value.
-- Used infix like
--
-- >>> $(mkRelFile "foo/a.txt") `asWithin` $(mkRelDir "foo")
--
asWithin :: MonadThrow m => Path a t -> Path a Dir -> m (Within a (Path Rel t))
asWithin x y = stripProperPrefix y x >>= \z -> return (WithinT (EnvT y (Identity z)))

-- | Synonym for `flip env`, put a relative path inside a directory.
--
-- >>> $(mkRelFile "a.txt") `within` $(mkRelDir "foo")
within :: a -> Path b Dir -> Within b a
within x y = WithinT (EnvT y (Identity x))

-- | Turns an `Env` directory containing a path into a single path.
fromWithin :: Within a (Path Rel t) -> Path a t
fromWithin = liftA2 (</>) ask extract

instance Eq t => Eq (Within b t) where
  WithinT (EnvT e (Identity a)) == WithinT (EnvT e' (Identity a')) = e == e' && a == a'

instance Hashable t => Hashable (Within b t) where
  hashWithSalt n (WithinT (EnvT e (Identity a))) = n `hashWithSalt` e `hashWithSalt` a

instance Show t => Show (Within b t) where
  show (WithinT (EnvT e a)) = show e ++ "/" ++ show a

-- | Switch the outer part of a `Within` value to a new directory, synonym for localDir . const
blinkLocalDir :: Path b Dir -> WithinT a w t -> WithinT b w t
blinkLocalDir = localDir . const

-- | Switch the outer part of a `Within` value to a new directory and map the inner at the same time.
blinkAndMap :: Functor w => Path b Dir -> (s -> t) -> WithinT a w s -> WithinT b w t
blinkAndMap k g = blinkLocalDir k . fmap g

-- | Switch the outer part of a `Within` value to a new directory and mapM the inner at the same time.
blinkAndMapM :: (Monad m, Traversable w) => Path b Dir -> (s -> m t) -> WithinT a w s -> m (WithinT b w t)
blinkAndMapM k g = mapM g . blinkLocalDir k

-- | MapM the outer and inner part of a `Within` value at the same time.
localDirAndMapM :: (Monad m, Traversable w) => (Path b Dir -> m (Path c Dir)) -> (s -> m t) -> WithinT b w s -> m (WithinT c w t)
localDirAndMapM f g = localDirM f <=< mapM g
