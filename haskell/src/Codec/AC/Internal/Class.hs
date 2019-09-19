-- |
-- Module      :  Codec.AC.Internal.Class
-- Copyright   :  Konrad Kleczkowski 2019
-- License     :  MIT
--
-- Maintainer  :  konrad.kleczkowski@gmail.com
-- Stability   :  experimental
-- Portability :  unknown
--
-- Provides an interface to monads that can do
-- encoding and decoding.
--
module Codec.AC.Internal.Class
  ( BaseCoder(..)
  , MonadEncoder(..)
  , MonadDecoder(..)
  , narrow
  )
where

import           Codec.AC.Internal.Arithmetic
import           Codec.AC.Internal.Symbol

-- | Monads that can query and update frequency table and operate on interval.
class (Monad m) => BaseCoder m where
  -- | Finds a cumulative frequency from @0@ to @key@ (inclusive).
  query :: Symbol -> m Word
  -- | Finds a symbol @k@ for given @c@ such that @query (pred k) <= c < query k@.
  iquery :: Word -> m Symbol
  -- | Updates a frequency table with new occurrence of symbol.
  update :: Symbol -> m ()
  -- | Gets an interval.
  getInterval :: m Interval
  -- | Puts an interval.
  putInterval :: Interval -> m ()

-- | Monads that are encoders.
class (BaseCoder m) => MonadEncoder m where
  -- | Gets a follow bit counter.
  getFollowBits :: m Int
  -- | Sets a follow bit counter.
  putFollowBits :: Int -> m ()

-- | Monads that are decoders.
class (BaseCoder m) => MonadDecoder m where
  -- | Gets a partial message tag.
  getMessageTag :: m Fraction
  -- | Updates a partial message tag with new bit.
  updateMessageTag :: Bool -> m ()
  -- | Modifies a partial message tag using updating function.
  adjustMessageTag :: (Fraction -> Fraction) -> m ()

-- | Narrows the interval of coder.
narrow :: (BaseCoder m) => Symbol -> m Interval
narrow s = do
  (l, r) <- getInterval
  freqL  <- fromIntegral <$> (if s == minBound then return 0 else query (pred s))
  freqR  <- fromIntegral <$> query s
  total  <- fromIntegral <$> query maxBound
  let range = getFixed r - getFixed l + 1
      l'    = getFixed l + range * freqL `div` total
      r'    = getFixed l + range * freqR `div` total - 1
  return $! (Fixed l', Fixed r')
