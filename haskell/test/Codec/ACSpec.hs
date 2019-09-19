module Codec.ACSpec (
  spec
) where

import qualified Codec.AC as AC
import           Control.Monad.Trans
import           Data.ByteString.Arbitrary
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import           Test.Hspec
import           Test.QuickCheck
import           Test.QuickCheck.Monadic

prop_codecIdentity :: ArbByteString -> Property
prop_codecIdentity (ABS bs) = monadicIO $ do
  compressed <- lift $ AC.compress (L.fromStrict bs)
  bs' <- lift $ AC.decompress compressed
  assert $ (L.toStrict bs') == bs

spec :: Spec
spec = do
  it "decompress . compress == id" $ property prop_codecIdentity
