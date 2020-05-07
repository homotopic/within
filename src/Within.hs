{-# LANGUAGE DeriveGeneric #-}
module Within (
  Within(..)
, fromWithin
, toWithin
, within
, asWithin
, whatLiesWithin
, mapWithin
, mapWithinT
, moveWithin
, moveWithinT
, blinkWithin
, moveAndMapT
, blinkAndMapT
) where

import Control.Monad.Catch
import Data.Typeable
import Data.Hashable
import GHC.Generics
import Path

-- | The `Within` type represents a relative `Path` inside a directory `Path`.
-- The two halves can be manipulated independently.
newtype Within a t = Within (Path a Dir, Path Rel t)
  deriving (Typeable, Generic, Eq, Show)

instance Hashable (Within a t) where
  hashWithSalt n w = hashWithSalt n (fromWithin w)

instance Ord (Within a t) where
  compare (Within (x, y)) (Within (x',y')) = compare x x' <> compare y y'

-- | Convert a `Within` to a `Path` by joining it with a path separator.
fromWithin :: Within a t -> Path a t
fromWithin (Within (x,y)) = x </> y

-- | Convert a directory `Path` and a relative `Path` to a `Within`
toWithin :: Path a Dir -> Path Rel t -> Within a t
toWithin = flip within

-- | Infix version of `toWithin`, e.g "file.txt \`within\` myDir"
within :: Path Rel t -> Path a Dir -> Within a t
within y x = Within (x,y)

-- | Attempts to convert a `Path` to a `Within` by treating it as if it were
-- within the second argument. Used infix as "myParentDir\/foo\/file.txt \`asWithin\` myParentDir"
asWithin :: MonadThrow m => Path a t -> Path a Dir -> m (Within a t)
asWithin x y = stripProperPrefix y x >>= \z -> return (Within (y, z))

-- | Extracts the inner path.
whatLiesWithin :: Within a t -> Path Rel t
whatLiesWithin (Within (_,y)) = y

-- | Map the inner part of the `Within` value to a new `Path`.
mapWithin :: (Path Rel s -> Path Rel t) -> Within a s -> Within a t
mapWithin f (Within (x,y)) = Within (x, f y)

-- | Map the inner part of the `Within` value to a new `Path` with an operation that may throw.
mapWithinT :: MonadThrow m => (Path Rel s -> m (Path Rel t)) -> Within a s -> m (Within a t)
mapWithinT f (Within (x,y)) = f y >>= \z -> return (Within (x, z))

-- | Switch the outer part of a `Within` value to a new directory immediately.
blinkWithin :: Path b Dir -> Within a t -> Within b t
blinkWithin = moveWithin . const

-- | Map the outer part of a `Within` value via a function that changes the directory.
moveWithin :: (Path a Dir -> Path b Dir) -> Within a t -> Within b t
moveWithin f (Within (x,y)) = Within (f x, y)

-- | Map the outer part of a `Within` value via a function that changes the directory with an operation that may throw.
moveWithinT :: MonadThrow m => (Path a Dir -> m (Path b Dir)) -> Within a t -> m (Within b t)
moveWithinT f (Within (x,y)) = f x >>= \z -> return (Within (z,y))

-- | `blinkWithin` and `mapWithinT` simultaneously.
blinkAndMapT :: MonadThrow m => Path b Dir -> (Path Rel s -> m (Path Rel t)) -> Within a s -> m (Within b t)
blinkAndMapT k g (Within (_,y)) = do
  y' <- g y
  return $ Within (k, y')

-- | `moveWithinT` and `mapWithinT` simultaneously.
moveAndMapT :: MonadThrow m => (Path a Dir -> m (Path b Dir)) -> (Path Rel s -> m (Path Rel t)) -> Within a s -> m (Within b t)
moveAndMapT f g (Within (x,y)) = do
  x' <- f x
  y' <- g y
  return $ Within (x', y')
