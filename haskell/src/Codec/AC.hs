-- |
-- Module      :  Codec.AC
-- Copyright   :  Konrad Kleczkowski 2019
-- License     :  MIT
--
-- Maintainer  :  konrad.kleczkowski@gmail.com
-- Stability   :  experimental
-- Portability :  unknown
--
-- Main facade of aritmetic codec.
--
module Codec.AC
  ( hCompress
  , hDecompress
  , compress
  , decompress
  )
where

import           Control.Lens
import           Control.Monad.Primitive
import           Control.Monad.Trans
import qualified Codec.AC.Internal             as I
import qualified Codec.AC.Encoder              as E
import qualified Codec.AC.Decoder              as D
import qualified Data.ByteString.Lazy          as L
import           Pipes
import qualified Pipes.ByteString              as PB
import           System.IO                      ( Handle )

-- | Compresses data incoming from one handle to another.
hCompress
  :: (PrimMonad m, MonadIO m)
  => Handle -- ^ Input handle.
  -> Handle -- ^ Output handle.
  -> m ()
{-# INLINABLE hCompress #-}
hCompress hIn hOut = I.runEncoder $ runEffect (bytesOut >-> PB.toHandle hOut)
 where
  bytesOut = view (I.bitPack . PB.pack)
                  (I.fromHandleEOF hIn >-> I.chunkToSymbols >-> E.encode)
  {-# INLINE bytesOut #-}

-- | Decompresses data incoming from one handle to another.
hDecompress
  :: (PrimMonad m, MonadIO m)
  => Handle -- ^ Input handle.
  -> Handle -- ^ Output handle.
  -> m ()
{-# INLINABLE hDecompress #-}
hDecompress hIn hOut =
  I.runDecoder $ runEffect $ view PB.pack bytesOut >-> PB.toHandle hOut
 where
  bytesOut = view (PB.unpack . I.bitUnpack) (I.fromHandleEOF hIn) >-> D.decode
  {-# INLINE bytesOut #-}

-- | Compresses lazy byte string. (Only for testing purposes).
compress :: (PrimMonad m) => L.ByteString -> m L.ByteString
compress input = I.runEncoder $ PB.toLazyM bytesOut 
 where
  bytesOut = view (I.bitPack . PB.pack)
                  (I.fromLazyEOF input >-> E.encode)
  {-# INLINE bytesOut #-}

-- | Decompresses lazy byte string. (Only for testing purposes).
decompress :: (PrimMonad m) => L.ByteString -> m L.ByteString
decompress input =
  I.runDecoder $ PB.toLazyM (view PB.pack bytesOut)
 where
  bytesOut = view (PB.unpack . I.bitUnpack) (PB.fromLazy input) >-> D.decode
  {-# INLINE bytesOut #-}
