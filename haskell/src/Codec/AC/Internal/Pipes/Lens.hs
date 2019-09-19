-- |
-- Module      :  Codec.AC.Internal.Pipes.Lens
-- Copyright   :  Konrad Kleczkowski 2019
-- License     :  MIT
--
-- Maintainer  :  konrad.kleczkowski@gmail.com
-- Stability   :  experimental
-- Portability :  unknown
--
-- Provides improper lenses that packs and unpacks byte streams into bit streams.
--
module Codec.AC.Internal.Pipes.Lens
  ( bitPack
  , bitUnpack
  )
where

import           Control.Lens
import           Data.Bits
import           Data.Word                      ( Word8 )
import           Pipes
import qualified Pipes.Group                   as PG

-- | Improper lens that focuses bit produces as byte producer.
bitPack :: (Monad m) => Lens' (Producer Bool m a) (Producer Word8 m a)
bitPack k p = fmap _unpack (k (_pack p))

-- | Improper lens that focuses byte producer as bit producer.
bitUnpack :: (Monad m) => Lens' (Producer Word8 m a) (Producer Bool m a)
bitUnpack k p = fmap _pack (k (_unpack p))

_pack :: (Monad m) => Producer Bool m a -> Producer Word8 m a
_pack p = PG.folds step (0, 0) finish (p ^. PG.chunksOf 8)
 where
  step (n, acc) b = (n + 1, acc .|. ((fromEnum b) `shiftL` (7 - n)))
  finish (_, acc) = fromIntegral acc

_unpack :: (Monad m) => Producer Word8 m a -> Producer Bool m a
_unpack p = for p (\b -> Pipes.each (step b))
 where
  step b = map (b `testBit`) (reverse [0 .. 7])
  {-# INLINE step #-}
