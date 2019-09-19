-- |
-- Module      :  Codec.AC.Internal.Arithmetic
-- Copyright   :  Konrad Kleczkowski 2019
-- License     :  MIT
--
-- Maintainer  :  konrad.kleczkowski@gmail.com
-- Stability   :  experimental
-- Portability :  unknown
--
-- Provides an implementation of low-precision 
-- integer based arithmetic.
--
module Codec.AC.Internal.Arithmetic
  ( Fraction
  , Interval
  , fracWidth
{-, fullRange
  , mask
  , half
  , quarter
  , threeQuarters
-}, unit
  , module Numeric.Fixed
  )
where

import           Numeric.Fixed (Fixed (..))

-- | An integer based fraction.
type Fraction = Fixed

-- | An interval.
type Interval = (Fraction, Fraction)

-- | Returns the width of fractional part
fracWidth :: Int
{-# INLINE fracWidth #-}
fracWidth = 16 
{-
-- | Returns the number @1.0@.
fullRange :: Fraction
{-# INLINE fullRange #-}
fullRange = 1 `shiftL` fracWidth

mask :: Fraction
{-# INLINE mask #-}
mask = fullRange - 1

-- | Returns the number @0.5@.
half :: Fraction
{-# INLINE half #-}
half = fullRange `shiftR` 1

-- | Returns the number @0.25@.
quarter :: Fraction
{-# INLINE quarter #-}
quarter = half `shiftR` 1

-- | Returns the number @0.75@.
threeQuarters :: Fraction
{-# INLINE threeQuarters #-}
threeQuarters = half + quarter
-}
-- | Returns the unit interval.
unit :: Interval
{-# INLINE unit #-}
unit = (0, 1.0 - Fixed 1)

