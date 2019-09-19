-- |
-- Module      :  Codec.AC.Decoder
-- Copyright   :  Konrad Kleczkowski 2019
-- License     :  MIT
--
-- Maintainer  :  konrad.kleczkowski@gmail.com
-- Stability   :  experimental
-- Portability :  unknown
--
-- Provides an implementation of arithmetic decoder.
--
module Codec.AC.Decoder
  ( decode
  )
where

import           Codec.AC.Internal
import           Control.Monad
import           Data.Word (Word8)
import           Pipes

decode :: (MonadDecoder m) => Pipe Bool Word8 m ()
decode = initDecoder >> loop

initDecoder :: (MonadDecoder m) => Pipe Bool Word8 m ()
initDecoder = replicateM_ 16 (await >>= lift . updateMessageTag)

loop :: (MonadDecoder m) => Pipe Bool Word8 m ()
loop = do
  s      <- lift decodeSymbol
  (l, r) <- lift $ narrow s
  lift $ update s
  scale (l, r)
  case s of
    Byte b -> yield b >> loop
    EndOfInput -> return ()

decodeSymbol :: (MonadDecoder m) => m Symbol
decodeSymbol = do
  t      <- getMessageTag
  total  <- fromIntegral <$> query maxBound
  (l, r) <- getInterval
  let range  = getFixed r - getFixed l + 1
      offset = getFixed t - getFixed l
      cum    = fromIntegral $ ((offset + 1) * total - 1) `div` range
  iquery cum

scale :: (MonadDecoder m) => Interval -> Pipe Bool Word8 m ()
scale (l, r)
  | r < 0.5 = do
    lift . updateMessageTag =<< await
    scale (2 * l, 2 * r + Fixed 1)
  | l >= 0.5 = do
    lift $ adjustMessageTag (+ (-0.5))
    lift . updateMessageTag =<< await
    scale (2 * l - 1, 2 * r - 1 + Fixed 1)
  | l >= 0.25 && r < 0.75 = do
    lift $ adjustMessageTag (+ (-0.25))
    lift . updateMessageTag =<< await
    scale (2 * l - 0.5, 2 * r - 0.5 + Fixed 1)
  | otherwise = lift $ putInterval (l, r)
