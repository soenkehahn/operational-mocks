{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}

module Control.Monad.Operational.Mocks where

import           Control.Exception
import           Control.Monad.Operational
import           Data.Type.Equality
import           Data.Typeable
import           Prelude hiding (gcd, log)
import           Test.Hspec
import           Unsafe.Coerce

data Mock' prim a where
  (:>>>=) :: (Show (prim a), Show (prim b)) =>
    MockedPrim prim a -> Mock' prim b -> Mock' prim b
  Result :: a -> Mock' prim a
infixr 8 :>>>=

data MockedPrim prim a where
  (:~>) :: Show (prim a) => prim a -> a -> MockedPrim prim a
infixr :~>

testWithMock :: (CommandEq prim, Eq a, Show a) => Program prim a -> Mock' prim a -> IO ()
testWithMock program mock = case (view program, mock) of
  (programCommand :>>= nextProgram, mockCommand :~> mockResult :>>>= nextMock) -> do
    case commandEq mockCommand programCommand of
      Left (a, b) -> throwIO $ ErrorCall $
        "expected: call to " ++ a ++ ", got: " ++ b
      Right refl -> do
        testWithMock (nextProgram (castWith refl mockResult)) nextMock
  (Return programResult, Result mockResult) ->
    programResult `shouldBe` mockResult
  (Return _, mockCommand :~> _ :>>>= _) -> throwIO $ ErrorCall $
    "expected: call to " ++ showConstructor mockCommand ++
    ", got: function returned"
  (programCommand :>>= _, Result _) -> throwIO $ ErrorCall $
    "expected: end of function, got: " ++
    showConstructor programCommand

-- * CommandEq

class CommandEq command where
  commandEq :: command a -> command b -> Either (String, String) (a :~: b)
  showConstructor :: command a -> String

eitherEq :: forall a b . (Typeable a, Typeable b, Eq a) =>
  a -> b -> Either () (a :~: b)
eitherEq a b = case maybeRefl (Proxy :: Proxy a) (Proxy :: Proxy b) of
  Nothing -> Left ()
  Just refl -> if a == castWith (sym refl) b
    then Right refl
    else Left ()

maybeRefl :: forall a b . (Typeable a, Typeable b) =>
  Proxy a -> Proxy b -> Maybe (a :~: b)
maybeRefl Proxy Proxy = case cast (undefined :: a) :: Maybe b of
  Nothing -> Nothing
  Just _ -> Just (unsafeCoerce Refl)
