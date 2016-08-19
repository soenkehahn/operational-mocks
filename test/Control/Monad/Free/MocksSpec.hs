{-# LANGUAGE GADTs #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Control.Monad.Free.MocksSpec where

import Control.Monad.Free
import Control.Exception
import Test.Hspec
import Prelude hiding (getLine)

import Control.Monad.Free.Mocks

-- * Functor
data ConsoleF a where
  GetLine :: (String -> a) -> ConsoleF a
  WriteLine :: String -> a -> ConsoleF a

deriving instance Functor ConsoleF

instance HasMock ConsoleF where
  mockExtract (GetLine f) = Right $ W f
  mockExtract (WriteLine _ a) = Left a

type Console = Free ConsoleF

instance ShowConstructor ConsoleF where
  showConstructor (GetLine _) = "GetLine"
  showConstructor (WriteLine _ _) = "WriteLine"

instance Assert ConsoleF where
   assert (GetLine _) (GetLine _) = Just (return ())
   assert (WriteLine s _) (WriteLine s2 _) = Just (s `shouldBe` s2)
   assert _ _ = Nothing

-- * Primitive ops
getLineF :: ConsoleF String
getLineF = GetLine id

writeLineF :: String -> ConsoleF ()
writeLineF s = WriteLine s ()

getLine :: Console String
getLine = liftF getLineF

writeLine :: String -> Console ()
writeLine = liftF . writeLineF

-- * programs for testing
getAndWrite :: Console ()
getAndWrite = getLine >>= writeLine . reverse

spec :: Spec
spec = describe "Free Mocks Specs" $ do
  it "should allow to mock a sequence of operations" $ do
    let mockConsole =
          getLineF `returns` "say" `andThen`
          writeLineF "yas" `returns` () `andThen`
          []
    testFreeWithMock getAndWrite mockConsole

  it "catch unexpected calls to primitives" $ do
    let mockConsole =
          writeLineF "say" `returns` () `andThen`
          writeLineF "yas" `returns` () `andThen`
          []
        test = testFreeWithMock getAndWrite mockConsole
    test `shouldThrow` errorCall "expected: call to WriteLine, got: GetLine"

  it "catches unexpected superfluous calls to primitives" $ do
    let test = testFreeWithMock getAndWrite $
          getLineF `returns` "foo" `andThen`
          []
    test `shouldThrow` errorCall "expected: function returns, got: WriteLine"

  it "catches missing calls to primitives" $ do
    let test = testFreeWithMock getAndWrite $
               getLineF `returns` "foo" `andThen`
               writeLineF "oof" `returns` () `andThen`
               getLineF `returns` "bar" `andThen`
               []
    test `shouldThrow` errorCall "expected: call to a primitive, got: function returns"

  it "catches wrong calls to constructors" $ do
    let test = testFreeWithMock getAndWrite $
               getLineF `returns` "foo" `andThen`
               writeLineF "foo" `returns` () `andThen`
               []
    test `shouldThrow` (\(_ :: SomeException) -> True)
