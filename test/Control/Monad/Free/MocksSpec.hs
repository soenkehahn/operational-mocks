{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE InstanceSigs #-}

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
  mockE (GetLine f) = Right $ W f
  mockE (WriteLine _ a) = Left a


instance Show (ConsoleF a) where
  show (GetLine _) = "GetLine"
  show (WriteLine _ _) = "WriteLine"

type Console = Free ConsoleF

instance ShowConstructor ConsoleF where
  showConstructor (GetLine _) = "GetLine"
  showConstructor (WriteLine _ _) = "WriteLine"

instance Assert ConsoleF where
   assert (GetLine _) (GetLine _) = pure $ Just ()
   assert (WriteLine s _) (WriteLine s2 _) = fmap Just (s `shouldBe` s2)
   assert _ _ = pure Nothing

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
    let mockConsole = mock $
          getLineF `returns` "say" `andThen`
          writeLineF "yas" `returns` () `andThen`
          []
    testFreeWithMock getAndWrite mockConsole

  it "catch unexpected calls to primitives" $ do
    let mockConsole = mock $
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
