{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
-- |
-- Module      :  Codec.AC.Internal.Encoder
-- Copyright   :  Konrad Kleczkowski 2019
-- License     :  MIT
--
-- Maintainer  :  konrad.kleczkowski@gmail.com
-- Stability   :  experimental
-- Portability :  unknown
--
-- Provides an implementation of encoder monad.
--
module Codec.AC.Internal.Encoder
  ( Encoder
  , runEncoder
  )
where

import           Codec.AC.FreqTable             ( FreqTable )
import qualified Codec.AC.FreqTable            as FT
import           Codec.AC.Internal.Arithmetic
import           Codec.AC.Internal.Class
import           Control.Monad
import           Control.Monad.Primitive
import           Control.Monad.Trans
import           Control.Monad.Trans.Reader
import           Data.Primitive

-- | An encoder state data type.
data EncoderState s
  = EncoderState
  { model       :: FreqTable s        -- ^ A mutable frequency table.
  , interval    :: MutVar s Interval  -- ^ A reference to interval
  , followBits  :: MutVar s Int       -- ^ A follow bits counter.
  }

-- | An encoding monad.
type Encoder m = ReaderT (EncoderState (PrimState m)) m

instance (PrimMonad m, s ~ PrimState m) => BaseCoder (ReaderT (EncoderState s) m) where
  query s = fromIntegral <$> (lift . FT.query (fromEnum s) =<< asks model)
  {-# INLINE query #-}
  iquery c = toEnum <$> (lift . FT.iquery (fromIntegral c) =<< asks model)
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

instance (PrimMonad m, s ~ PrimState m) => MonadEncoder (ReaderT (EncoderState s) m) where
  getFollowBits = lift . readMutVar =<< asks followBits
  {-# INLINE getFollowBits #-}
  putFollowBits f = lift . flip writeMutVar f =<< asks followBits
  {-# INLINE putFollowBits #-}

-- | Runs encoder monad.
runEncoder :: (PrimMonad m) => Encoder m a -> m a
runEncoder m = do
  model <- FT.new 9
  forM_ [0 .. 511] $ \i -> FT.update i (+ 1) model
  interval   <- newMutVar unit
  followBits <- newMutVar 0
  let state = EncoderState model interval followBits
  runReaderT m state
