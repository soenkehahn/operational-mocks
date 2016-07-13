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

data Mock prim a where
  AndThen :: MockedPrim prim a -> Mock prim b -> Mock prim b
  Result :: a -> Mock prim a

andThen :: MockedPrim prim a -> Mock prim b -> Mock prim b
andThen = AndThen
infixr 8 `andThen`

data MockedPrim prim a where
  Returns :: prim a -> a -> MockedPrim prim a

returns :: prim a -> a -> MockedPrim prim a
returns = Returns

testWithMock :: (CommandEq prim, Eq a, Show a) => Program prim a -> Mock prim a -> IO ()
testWithMock program mock = case (view program, mock) of
  (programCommand :>>= nextProgram, mockCommand `Returns` mockResult `AndThen` nextMock) -> do
    case commandEq mockCommand programCommand of
      Left () -> throwIO $ ErrorCall $
        "expected: call to " ++ showConstructor mockCommand ++
        ", got: " ++ showConstructor programCommand
      Right refl -> do
        testWithMock (nextProgram (castWith refl mockResult)) nextMock
  (Return programResult, Result mockResult) ->
    programResult `shouldBe` mockResult
  (Return _, mockCommand `Returns` _ `AndThen` _) -> throwIO $ ErrorCall $
    "expected: call to " ++ showConstructor mockCommand ++
    ", got: function returned"
  (programCommand :>>= _, Result _) -> throwIO $ ErrorCall $
    "expected: end of function, got: " ++
    showConstructor programCommand

-- * CommandEq

class CommandEq command where
  commandEq :: command a -> command b -> Either () (a :~: b)
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
