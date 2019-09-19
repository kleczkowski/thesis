module Main
  ( main
  )
where

import qualified Codec.AC as AC
import           Criterion.Main
import qualified Data.ByteString.Lazy as L
import           Data.ByteString.Random

main :: IO ()
main = 
  defaultMain [
    env setupEnv $ \ ~(sbs, ebs, rbs, lbs) -> 
    bgroup "main" 
    [
      bgroup "singleByte" 
      [ bench "encode" $ nfIO $ AC.compress sbs
      , bench "identity" $ nfIO $ AC.decompress =<< AC.compress sbs
      ]
    , bgroup "eachByte" 
      [ bench "encode" $ nfIO $ AC.compress ebs
      , bench "identity" $ nfIO $ AC.decompress =<< AC.compress ebs
      ]
    , bgroup "random" 
      [ bench "encode" $ nfIO $ AC.compress rbs
      , bench "identity" $ nfIO $ AC.decompress =<< AC.compress rbs
      ]
    , bgroup "lorem" 
      [ bench "encode" $ nfIO $ AC.compress lbs
      , bench "identity" $ nfIO $ AC.decompress =<< AC.compress lbs
      ]
    ]
  ]

setupEnv :: IO (L.ByteString, L.ByteString, L.ByteString, L.ByteString)
setupEnv = do
  let size = 4096
  sbs <- singleByteBS size
  ebs <- eachByteBS size
  rbs <- randomBS size
  lbs <- L.readFile "bench/lorem.txt"
  return (sbs, ebs, rbs, lbs)

singleByteBS :: Int -> IO L.ByteString
singleByteBS size = return $ L.replicate (fromIntegral size) 15

eachByteBS :: Int -> IO L.ByteString
eachByteBS size = return $ L.pack $ take size (cycle [0..255])

randomBS :: Int -> IO L.ByteString
randomBS size = L.fromStrict <$> (random (fromIntegral size))

