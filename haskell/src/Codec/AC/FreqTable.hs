-- |
-- Module      :  Codec.AC.FreqTable
-- Copyright   :  Konrad Kleczkowski 2019
-- License     :  MIT
--
-- Maintainer  :  konrad.kleczkowski@gmail.com
-- Stability   :  experimental
-- Portability :  unknown
--
-- Provides an implementation of frequency table
-- that is based on binary indexed tree.
--
module Codec.AC.FreqTable
  ( FreqTable
  , new
  , query
  , iquery
  , update
  , scale
  )
where

import           Control.Monad
import           Control.Monad.Primitive

import           Data.Bits
import           Data.Vector.Unboxed.Mutable    ( MVector )
import qualified Data.Vector.Unboxed.Mutable   as MV

-- | A frequency table data type.
newtype FreqTable s = FT (MVector s Int)

-- | Creates a new instance of a frequency table.
new
  :: (PrimMonad m)
  => Int -- ^ Depth of tree. Must be non-negative.
  -> m (FreqTable (PrimState m))
{-# INLINABLE new #-}
new depth | depth < 0 = error "Codec.AC.FreqTable.new: negative depth"
          | otherwise = FT <$> MV.new (2 ^ depth + 1)

-- | \( O ( \log n ) \). Computes a cumulative frequency
-- from zero to given key inclusive.
query
  :: (PrimMonad m)
  => Int -- ^ The key.
  -> FreqTable (PrimState m)
  -> m Int
{-# INLINABLE query #-}
query key (FT vec) = sum <$> mapM (MV.read vec) indices
 where
  indices = takeWhile (> 0) $ iterate (\i -> i - lsb i) (key + 1)
  {-# INLINE indices #-}

-- | \( O (\log n) \). Finds a key @k@ such that
-- @query (pred k) <= c < query k@
-- for given @c@.
iquery
  :: (PrimMonad m)
  => Int -- ^ The cumulative frequency @c@.
  -> FreqTable (PrimState m)
  -> m Int
{-# INLINABLE iquery #-}
iquery c (FT vec) = go c (n - 1) 0 vec
 where
  n = MV.length vec
  go :: (PrimMonad m) => Int -> Int -> Int -> MVector (PrimState m) Int -> m Int
  go c mask offset vec
    | idx < 0 || idx > n = error
      "Codec.AC.FreqTable.iquery: index out of bounds"
    | mask == 0 = return idx 
    | otherwise = do
      treeC <- MV.read vec idx
      case compare c treeC of
        LT -> go c (mask `div` 2) offset vec
        EQ -> return $ idx
        GT -> go (c - treeC) (mask `div` 2) (offset + mask) vec
   where
    idx = mask + offset

-- | \( O (\log n) \). Updates cumulative frequencies from key to 
-- the upper bound of the frequency table.
update
  :: (PrimMonad m) 
  => Int -- ^ The key.
  -> (Int -> Int) -- ^ The updating function.
  -> FreqTable (PrimState m) 
  -> m ()
{-# INLINABLE update #-}
update key f (FT vec) = mapM_ (MV.modify vec f) indices
 where
  n       = MV.length vec
  indices = takeWhile (< n) $ iterate (\i -> i + lsb i) (key + 1)
  {-# INLINE n #-}
  {-# INLINE indices #-}

-- | \( O (n \log n) \). Scales frequencies by half down.
scale :: (PrimMonad m) => FreqTable (PrimState m) -> m ()
{-# INLINABLE scale #-}
scale ft@(FT vec) = forM_ (reverse [0 .. n - 2]) $ \i -> do
  f <- freq i ft
  let f' = negate $ f `div` 2
  update i (+ f') ft
  where
    n = MV.length vec
    freq :: (PrimMonad m) => Int -> FreqTable (PrimState m) -> m Int
    freq s ft = liftM2 (-) (query s ft) (query (s - 1) ft)

lsb :: Int -> Int
{-# INLINE lsb #-}
lsb i = i .&. (-i)
