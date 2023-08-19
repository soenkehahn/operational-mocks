{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeOperators #-}

module Control.Monad.Operational.Mocks where

import Control.Applicative
import Control.Monad.Operational qualified as Op
import Data.Typeable
import Test.Hspec
import Prelude hiding (gcd, log)

data Mock primitive a where
  Cons :: primitive tmp -> tmp -> Mock primitive b -> Mock primitive b
  Nil :: a -> Mock primitive a

instance Functor (Mock primitive) where
  fmap :: (a -> b) -> Mock primitive a -> Mock primitive b
  fmap f = \case
    Cons primitive tmp next -> Cons primitive tmp (fmap f next)
    Nil a -> Nil $ f a

instance Applicative (Mock primitive) where
  pure :: a -> Mock primitive a
  pure = Nil

  (<*>) :: Mock primitive (a -> b) -> Mock primitive a -> Mock primitive b
  a <*> b = concatViews a (\f -> fmap f b)

instance Monad (Mock primitive) where
  (>>=) :: Mock primitive a -> (a -> Mock primitive b) -> Mock primitive b
  a >>= b = concatViews a b

concatViews :: Mock primitive a -> (a -> Mock primitive b) -> Mock primitive b
concatViews a b = case a of
  Cons primitive tmp rest -> Cons primitive tmp (concatViews rest b)
  Nil tmp -> b tmp

returns :: primitive a -> a -> Mock primitive ()
returns primitive a = Cons primitive a (Nil ())

testWithMock ::
  (Show a, Eq a, CommandEq primitive, ShowConstructor primitive) =>
  Op.Program primitive a ->
  Mock primitive a ->
  IO ()
testWithMock real mock =
  go (Op.view real) mock
  where
    go ::
      (Show a, Eq a, CommandEq primitive, ShowConstructor primitive) =>
      Op.ProgramView primitive a ->
      Mock primitive a ->
      IO ()
    go real mock =
      case (real, mock) of
        (realPrimitive Op.:>>= realRest, Cons mockPrimitive tmp mockRest) -> do
          result <- commandEq realPrimitive mockPrimitive
          case result of
            Left () ->
              error $
                "expected: call to "
                  <> showConstructor mockPrimitive
                  <> ", got: "
                  <> showConstructor realPrimitive
            Right Refl -> do
              go (Op.view (realRest tmp)) mockRest
        (Op.Return real, Nil mocked) -> do
          real `shouldBe` mocked
        (realPrimitive Op.:>>= _, Nil _) ->
          error $
            "expected: function returns, got: "
              <> showConstructor realPrimitive
        (Op.Return _, Cons mockPrimitive _ _) ->
          error $
            "expected: call to "
              <> showConstructor mockPrimitive
              <> ", got: function returns"

class CommandEq command where
  commandEq :: command a -> command b -> IO (Either () (a :~: b))

class ShowConstructor f where
  showConstructor :: f a -> String
