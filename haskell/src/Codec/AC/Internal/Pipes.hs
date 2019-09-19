-- |
-- Module      :  Codec.AC.Internal.Pipes
-- Copyright   :  Konrad Kleczkowski 2019
-- License     :  MIT
--
-- Maintainer  :  konrad.kleczkowski@gmail.com
-- Stability   :  experimental
-- Portability :  unknown
--
-- Provides utilities for manipulating on pipes.
--
module Codec.AC.Internal.Pipes
  ( -- * Utilities
    fromHandleEOF
  , fromLazyEOF
  , chunkToSymbols
    -- * Re-exports
  , module Codec.AC.Internal.Pipes.Lens
  )
where

import           Codec.AC.Internal.Symbol
import           Codec.AC.Internal.Pipes.Lens
import           Control.Monad
import           Data.ByteString                ( ByteString )
import qualified Data.ByteString               as S
import qualified Data.ByteString.Lazy as L
import           Pipes
import qualified Pipes.Prelude                 as P
import           System.IO                      ( Handle )

-- | Returns a producer that yields 4K chunks of data from handle.
--
-- If EOF is encountered, empty chunk is yielded.
fromHandleEOF :: (MonadIO m) => Handle -> Producer ByteString m ()
{-# INLINABLE fromHandleEOF #-}
fromHandleEOF hIn = go
 where
  go = do
    chunk <- liftIO (S.hGet hIn 4096)
    yield chunk
    unless (S.null chunk) go

-- | Returns a producer that yields a symbol stream from lazy byte string.
fromLazyEOF :: (Monad m) => L.ByteString -> Producer Symbol m ()
{-# INLINABLE fromLazyEOF #-}
fromLazyEOF = L.foldr (\b m -> yield (Byte b) >> m) (yield EndOfInput) 

-- | Returns a pipe that converts chunks into symbols.
--
-- Empty chunk is converted to 'EndOfInput' symbol.
chunkToSymbols :: (Monad m) => Pipe ByteString Symbol m ()
{-# INLINABLE chunkToSymbols #-}
chunkToSymbols = P.mapFoldable convert
 where
  convert chunk | S.null chunk = [EndOfInput]
                | otherwise     = map Byte (S.unpack chunk)
