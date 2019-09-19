-- |
-- Module      :  Codec.AC.Encoder
-- Copyright   :  Konrad Kleczkowski 2019
-- License     :  MIT
--
-- Maintainer  :  konrad.kleczkowski@gmail.com
-- Stability   :  experimental
-- Portability :  unknown
--
-- Provides an implementation of arithmetic encoder.
--
module Codec.AC.Encoder
  ( encode
  )
where

import           Codec.AC.Internal
import           Control.Monad
import           Data.Monoid
import           Pipes
import qualified Pipes.Prelude                 as P

-- | Returns a pipe that encodes symbols into a stream of bits.
encode :: (MonadEncoder m) => Pipe Symbol Bool m ()
{-# INLINABLE encode #-}
encode =
  P.mapM
      (\s -> do
        (l, r) <- narrow s
        update s
        case s of
          EndOfInput -> liftM2 (++) (scale (l, r)) finalize
          _ -> scale (l, r)
      )
    >-> P.concat

-- | Renormalizes an interval.
scale :: (MonadEncoder m) => Interval -> m [Bool]
{-# INLINABLE scale #-}
scale i = do
  f <- getFollowBits
  let (i', f', acc) = go i f mempty
  putFollowBits f'
  putInterval i'
  return $! appEndo acc []
 where
  go i@(l, r) f acc
    | r < 0.5 = go (2 * l, 2 * r + Fixed 1)
                    0
                    (bitWithFollows False <> acc)
    | l >= 0.5 = go (2 * l - 1, 2 * r - 1 + Fixed 1)
                     0
                     (bitWithFollows True <> acc)
    | l >= 0.25 && r < 0.75 = go
      (2 * l - 0.5, 2 * r - 0.5 + Fixed 1)
      (f + 1)
      acc
    | otherwise = (i, f, acc)
    where bitWithFollows b = Endo (++ b : replicate f (not b))

-- | Finalizes bit stream, appending bits that decoder can end its work gratefully.
finalize :: (MonadEncoder m) => m [Bool]
finalize = do
  (l, _) <- getInterval
  f <- getFollowBits
  return $ if l < 0.25 
    then False : replicate (f + 1) True
    else True : replicate (f + 1) False
