{-# LANGUAGE GADTs #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Control.Monad.Free.MocksSpec where

import Control.Monad.Free
import Control.Exception
import Data.List
import Test.Hspec
import Prelude hiding (getLine)

import Control.Monad.Free.Mocks

-- * Functor
data ConsoleF a where
  GetLine :: (String -> a) -> ConsoleF a
  WriteLine :: String -> a -> ConsoleF a
  ReadLine :: Read r => (r -> a) -> ConsoleF a

deriving instance Functor ConsoleF

instance HasMock ConsoleF where
  mockExtract (GetLine f) = Right $ W f
  mockExtract (WriteLine _ a) = Left a
  mockExtract (ReadLine f) = Right $ W f

type Console = Free ConsoleF

instance ShowConstructor ConsoleF where
  showConstructor (GetLine _) = "GetLine"
  showConstructor (WriteLine _ _) = "WriteLine"
  showConstructor (ReadLine _) = "ReadLine"

instance Assert ConsoleF where
   assert (GetLine _) (GetLine _) = Just (return ())
   assert (WriteLine s _) (WriteLine s2 _) = Just (s `shouldBe` s2)
   assert (ReadLine _) (ReadLine _) = Just (return ())
   assert _ _ = Nothing

-- * Primitive ops
getLineF :: ConsoleF String
getLineF = GetLine id

writeLineF :: String -> ConsoleF ()
writeLineF s = WriteLine s ()

readLineF :: Read r => ConsoleF r
readLineF = ReadLine id

getLine :: Console String
getLine = liftF getLineF

writeLine :: String -> Console ()
writeLine = liftF . writeLineF

readLine :: Read r => Console r
readLine = liftF readLineF

-- * programs for testing
getAndWrite :: Console ()
getAndWrite = getLine >>= writeLine . reverse

readBool :: Console ()
readBool = do
  b <- readLine
  if b then do
    writeLine "foo"
  else do
    writeLine "bar"

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

  it "catches mismatching return types" $ do
    let test = testFreeWithMock readBool $
               readLineF `returns` (42 :: Int) `andThen`
               writeLineF "foo" `returns` () `andThen`
               []
    test `shouldThrow` (\(x :: SomeException) -> "type mismatch" `isInfixOf` show x)
