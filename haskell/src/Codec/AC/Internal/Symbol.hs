-- |
-- Module      :  Codec.AC.Internal.Symbol
-- Copyright   :  Konrad Kleczkowski 2019
-- License     :  MIT
--
-- Maintainer  :  konrad.kleczkowski@gmail.com
-- Stability   :  experimental
-- Portability :  unknown
--
-- Provides a symbol data type.
--
module Codec.AC.Internal.Symbol
  ( Symbol(..)
  )
where

import           Data.Word (Word8)

-- | A symbol.
data Symbol
  = Byte Word8 -- ^ Plain byte.
  | EndOfInput -- ^ Indicates the end of input.
  deriving (Eq, Show)

instance Ord Symbol where
  _          <= EndOfInput = True
  EndOfInput <= x          = EndOfInput == x
  (Byte b1)  <= (Byte b2)  = b1 <= b2

instance Enum Symbol where
  fromEnum (Byte b)   = fromIntegral b
  fromEnum EndOfInput = 256

  toEnum i | i >= 0 && i <= 255 = Byte (fromIntegral i)
           | i == 256           = EndOfInput
           | otherwise          = error $ "Codec.AC.Internal.Symbol.toEnum: out of bounds ("
                                        ++ show i
                                        ++ ")"

instance Bounded Symbol where
  minBound = Byte 0
  maxBound = EndOfInput
