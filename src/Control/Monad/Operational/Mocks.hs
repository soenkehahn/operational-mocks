{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}

module Control.Monad.Operational.Mocks where

import           Control.Exception
import           Control.Monad.Operational
import           Data.Type.Equality
import           Prelude hiding (gcd, log)
import           Test.Hspec

data Mock primitive a where
  AndThen :: MockedPrimitive primitive a -> Mock primitive b -> Mock primitive b
  TestResult :: (a -> IO ()) -> Mock primitive a

testResult :: (a -> IO ()) -> Mock primitive a
testResult = TestResult

result :: (Show a, Eq a) => a -> Mock primitive a
result mock = TestResult $ \ real ->
  real `shouldBe` mock

andThen :: MockedPrimitive primitive a -> Mock primitive b -> Mock primitive b
andThen = AndThen
infixr 8 `andThen`

data MockedPrimitive primitive a where
  TestPrimitive :: (forall a . primitive a -> IO (a :~: a')) -> a'
    -> MockedPrimitive primitive a'

testPrimitive :: (forall a . primitive a -> IO (a :~: a')) -> a'
  -> MockedPrimitive primitive a'
testPrimitive = TestPrimitive

testWithMock :: (Show a, Eq a, ShowConstructor primitive) =>
  Program primitive a -> Mock primitive a -> IO ()
testWithMock real mock = case (view real, mock) of
  (realCommand :>>= nextProgram, TestPrimitive predicate mockResult `AndThen` nextMock) -> do
    refl <- predicate realCommand
    testWithMock (nextProgram (castWith (sym refl) mockResult)) nextMock

  (Return realResult, TestResult predicate) -> do
    predicate realResult
  (Return _, _ `AndThen` _) -> throwIO $ ErrorCall $
    "expected: call to a primitive" ++
    ", got: function returns"
  (realCommand :>>= _, TestResult _) -> throwIO $ ErrorCall $
    "expected: function returns, got: " ++ showConstructor realCommand

-- * convenience

returns :: forall mockResult primitive .
  (CommandEq primitive, ShowConstructor primitive) =>
  primitive mockResult -> mockResult -> MockedPrimitive primitive mockResult
returns mockPrimitive = testPrimitive p
  where
    p :: primitive realResult -> IO (realResult :~: mockResult)
    p realPrimitive = do
      testResult <- commandEq realPrimitive mockPrimitive
      case testResult of
        Left () -> throwIO $ ErrorCall $
          "expected: call to " ++ showConstructor mockPrimitive ++
          ", got: " ++ showConstructor realPrimitive
        Right refl -> return refl

-- * CommandEq

class CommandEq command where
  commandEq :: command a -> command b -> IO (Either () (a :~: b))

class ShowConstructor f where
  showConstructor :: f a -> String
