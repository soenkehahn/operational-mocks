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

data Mock prim a where
  AndThen :: MockedPrim prim a -> Mock prim b -> Mock prim b
  Result :: a -> Mock prim a

andThen :: MockedPrim prim a -> Mock prim b -> Mock prim b
andThen = AndThen
infixr 8 `andThen`

data MockedPrim prim a where
  TestPrimitive :: (forall a . prim a -> IO (a :~: a')) -> a' -> MockedPrim prim a'

testPrimitive :: (forall a . prim a -> IO (a :~: a')) -> a' -> MockedPrim prim a'
testPrimitive = TestPrimitive

testWithMock :: (Show a, Eq a) => Program prim a -> Mock prim a -> IO ()
testWithMock program mock = case (view program, mock) of
  (programCommand :>>= nextProgram, TestPrimitive predicate mockResult `AndThen` nextMock) -> do
    refl <- predicate programCommand
    testWithMock (nextProgram (castWith (sym refl) mockResult)) nextMock

  (Return programResult, Result mockResult) ->
    programResult `shouldBe` mockResult
  (Return _, _ `AndThen` _) -> throwIO $ ErrorCall $
    "expected: call to another primitive" ++
    ", got: function returned"
  (_ :>>= _, Result _) -> throwIO $ ErrorCall $
    "expected: end of function, got: another primitive"

-- * convenience

returns :: forall mockResult prim . (CommandEq prim) =>
  prim mockResult -> mockResult -> MockedPrim prim mockResult
returns mockPrim mockResult = testPrimitive p mockResult
  where
    p :: prim realResult -> IO (realResult :~: mockResult)
    p programPrim = do
      testResult <- commandEq programPrim mockPrim
      case testResult of
        Left () -> throwIO $ ErrorCall $
          "expected: call to " ++ showConstructor mockPrim ++
          ", got: " ++ showConstructor programPrim
        Right refl -> do
          return refl

-- * CommandEq

class CommandEq command where
  commandEq :: command a -> command b -> IO (Either () (a :~: b))
  showConstructor :: command a -> String
