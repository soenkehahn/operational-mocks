{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}

module Control.Monad.Operational.Mocks where

import Control.Applicative
import Control.Monad.Operational qualified as Op
import Data.Maybe
import Data.Typeable
import Test.Hspec
import Prelude hiding (gcd, log)

data CommandCheck command a = CommandCheck
  { name :: Maybe String,
    check :: (forall other. command other -> IO (Either () (a :~: other)))
  }

data Mock command a where
  Cons ::
    CommandCheck command tmp ->
    tmp ->
    Mock command b ->
    Mock command b
  Nil :: a -> Mock command a

instance Functor (Mock command) where
  fmap :: (a -> b) -> Mock command a -> Mock command b
  fmap f = \case
    Cons commandCheck tmp next -> Cons commandCheck tmp (fmap f next)
    Nil a -> Nil $ f a

instance Applicative (Mock command) where
  pure :: a -> Mock command a
  pure = Nil

  (<*>) :: Mock command (a -> b) -> Mock command a -> Mock command b
  a <*> b = a >>= (\f -> fmap f b)

instance Monad (Mock command) where
  (>>=) :: Mock command a -> (a -> Mock command b) -> Mock command b
  a >>= b = case a of
    Cons commandCheck tmp rest -> Cons commandCheck tmp (rest >>= b)
    Nil tmp -> b tmp

commandSatisfies ::
  (forall other. command other -> IO (Either () (a :~: other))) ->
  a ->
  Mock command ()
commandSatisfies check a =
  Cons (CommandCheck {name = Nothing, check}) a (Nil ())

returns ::
  (CommandEq command) =>
  command a ->
  a ->
  Mock command ()
returns expected a =
  Cons
    (CommandCheck (Just $ showConstructor expected) (commandEq expected))
    a
    (Nil ())

testWithMock ::
  (Show a, Eq a, CommandEq command) =>
  Op.Program command a ->
  Mock command a ->
  IO ()
testWithMock real mock =
  go (Op.view real) mock
  where
    go ::
      (Show a, Eq a, CommandEq command) =>
      Op.ProgramView command a ->
      Mock command a ->
      IO ()
    go real mock =
      case (real, mock) of
        (realCommand Op.:>>= realRest, Cons commandCheck tmp mockRest) -> do
          result <- check commandCheck realCommand
          case result of
            Left () ->
              error $ case name commandCheck of
                Just name ->
                  "expected: call to "
                    <> name
                    <> ", got: "
                    <> showConstructor realCommand
                Nothing -> "predicate failed on: " <> showConstructor realCommand
            Right Refl -> do
              go (Op.view (realRest tmp)) mockRest
        (Op.Return real, Nil mocked) -> do
          real `shouldBe` mocked
        (realCommand Op.:>>= _, Nil _) ->
          error $
            "expected: function returns, got: "
              <> showConstructor realCommand
        (Op.Return _, Cons commandCheck _ _) ->
          error $
            case name commandCheck of
              Just name ->
                "expected: call to "
                  <> name
                  <> ", got: function returns"
              Nothing -> "expected: call to another command, got: function returns"

class CommandEq command where
  commandEq :: command a -> (forall other. command other -> IO (Either () (a :~: other)))

  showConstructor :: command a -> String
