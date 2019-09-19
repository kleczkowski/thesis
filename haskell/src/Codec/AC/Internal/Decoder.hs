{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
-- |
-- Module      :  Codec.AC.Internal.Decoder
-- Copyright   :  Konrad Kleczkowski 2019
-- License     :  MIT
--
-- Maintainer  :  konrad.kleczkowski@gmail.com
-- Stability   :  experimental
-- Portability :  unknown
--
-- Provides an implementation of decoder monad.
--
module Codec.AC.Internal.Decoder
  ( Decoder
  , runDecoder
  )
where

import           Codec.AC.FreqTable             ( FreqTable )
import qualified Codec.AC.FreqTable            as FT
import           Codec.AC.Internal.Arithmetic
import           Codec.AC.Internal.Class
import           Codec.AC.Internal.Symbol
import           Control.Monad
import           Control.Monad.Primitive
import           Control.Monad.Trans
import           Control.Monad.Trans.Reader
import           Data.Primitive

-- | A decoder state data type.
data DecoderState s
  = DecoderState
  { model       :: FreqTable s        -- ^ A mutable frequency table.
  , interval    :: MutVar s Interval  -- ^ A reference to interval.
  , messageTag  :: MutVar s Fraction  -- ^ A fragment of message tag.
  }

-- | A decoding monad.
type Decoder m = ReaderT (DecoderState (PrimState m)) m

instance (PrimMonad m, s ~ PrimState m) => BaseCoder (ReaderT (DecoderState s) m) where
  query s = fromIntegral <$> (lift . FT.query (fromEnum s) =<< asks model)
  {-# INLINE query #-}
  iquery c = do
    total <- fromIntegral <$> (lift . FT.query 256 =<< asks model)
    if c >= total then return EndOfInput else toEnum <$> (lift . FT.iquery (fromIntegral c) =<< asks model)
  {-# INLINE iquery #-}
  update s = do
    ft <- asks model
    lift $ FT.update (fromEnum s) (+ 1) ft
    total <- query maxBound
    when (total >= 2 ^ (fracWidth - 2)) $ FT.scale ft
  getInterval = lift . readMutVar =<< asks interval
  {-# INLINE getInterval #-}
  putInterval i = lift . flip writeMutVar i =<< asks interval
  {-# INLINE putInterval #-}

instance (PrimMonad m, s ~ PrimState m) => MonadDecoder (ReaderT (DecoderState s) m) where
  getMessageTag = lift . readMutVar =<< asks messageTag
  {-# INLINE getMessageTag #-}
  updateMessageTag b = do
    tag <- getFixed <$> getMessageTag
    let tag' = Fixed (2 * tag + fromIntegral (fromEnum b)) 
    lift . flip writeMutVar tag' =<< asks messageTag
  adjustMessageTag f = do
    tag <- asks messageTag
    lift (modifyMutVar tag f)

runDecoder :: (PrimMonad m) => Decoder m a -> m a
runDecoder m = do
  model <- FT.new 9
  forM_ [0 .. 511] $ \i -> FT.update i (+ 1) model
  interval   <- newMutVar unit
  messageTag <- newMutVar 0
  let state = DecoderState model interval messageTag
  runReaderT m state
