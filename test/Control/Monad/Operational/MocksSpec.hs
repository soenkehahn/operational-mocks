
module Control.Monad.Operational.MocksSpec where

import           Test.Hspec

import           Control.Monad.Operational.Mocks

spec = do
  describe "library" $ do
    it "exists" $ do
      return () :: IO ()
