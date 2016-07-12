{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StandaloneDeriving #-}

module Control.Monad.Operational.MocksSpec where

import           Control.Monad.Operational
import           Data.Typeable
import           Prelude hiding (getLine)
import           Test.Hspec

import           Control.Monad.Operational.Mocks

spec :: Spec
spec = do
  describe "testWithMock" $ do
    it "allows to test against a sequence of primitive operations" $ do
      testWithMock lineReverse $
        GetLine :~> "foo" :>>>=
        WriteLine "oof" :~> () :>>>=
        Result ()

    it "catches unexpected primitive calls" $ do
      let test = testWithMock lineReverse $
            GetLine :~> "foo" :>>>=
            GetLine :~> "foo" :>>>=
            Result ()
      test `shouldThrow` errorCall "expected: call to GetLine, got: WriteLine"

-- * primitives

type TestProgram = Program TestPrim

data TestPrim a where
  GetLine :: TestPrim String
  WriteLine :: String -> TestPrim ()

deriving instance Show (TestPrim a)

getLine :: TestProgram String
getLine = singleton GetLine

writeLine :: String -> TestProgram ()
writeLine = singleton . WriteLine

instance CommandEq TestPrim where
  commandEq GetLine GetLine = Right Refl
  commandEq (WriteLine a) (WriteLine b)
    | a == b = Right Refl
  commandEq a b = Left (showConstructor a, showConstructor b)

  showConstructor = \ case
    GetLine -> "GetLine"
    WriteLine _ -> "WriteLine"

-- * test programs

lineReverse :: TestProgram ()
lineReverse = do
  l <- getLine
  writeLine (reverse l)
