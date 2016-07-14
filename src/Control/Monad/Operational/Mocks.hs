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
  Result :: a -> Mock primitive a

andThen :: MockedPrimitive primitive a -> Mock primitive b -> Mock primitive b
andThen = AndThen
infixr 8 `andThen`

data MockedPrimitive primitive a where
  TestPrimitive :: (forall a . primitive a -> IO (a :~: a')) -> a' -> MockedPrimitive primitive a'

testPrimitive :: (forall a . primitive a -> IO (a :~: a')) -> a' -> MockedPrimitive primitive a'
testPrimitive = TestPrimitive

testWithMock :: (Show a, Eq a) => Program primitive a -> Mock primitive a -> IO ()
testWithMock real mock = case (view real, mock) of
  (realCommand :>>= nextProgram, TestPrimitive predicate mockResult `AndThen` nextMock) -> do
    refl <- predicate realCommand
    testWithMock (nextProgram (castWith (sym refl) mockResult)) nextMock

  (Return realResult, Result mockResult) ->
    realResult `shouldBe` mockResult
  (Return _, _ `AndThen` _) -> throwIO $ ErrorCall $
    "expected: call to another primitive" ++
    ", got: function returned"
  (_ :>>= _, Result _) -> throwIO $ ErrorCall $
    "expected: end of function, got: another primitive"

-- * convenience

returns :: forall mockResult primitive . (CommandEq primitive) =>
  primitive mockResult -> mockResult -> MockedPrimitive primitive mockResult
returns mockPrimitive mockResult = testPrimitive p mockResult
  where
    p :: primitive realResult -> IO (realResult :~: mockResult)
    p realPrimitive = do
      testResult <- commandEq realPrimitive mockPrimitive
      case testResult of
        Left () -> throwIO $ ErrorCall $
          "expected: call to " ++ showConstructor mockPrimitive ++
          ", got: " ++ showConstructor realPrimitive
        Right refl -> do
          return refl

-- * CommandEq

class CommandEq command where
  commandEq :: command a -> command b -> IO (Either () (a :~: b))
  showConstructor :: command a -> String
