{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Control.Monad.Operational.MocksSpec where

import Control.Applicative
import Control.Exception (ErrorCall (ErrorCall), Exception, catch, throwIO)
import Control.Monad
import Control.Monad.Operational
import Control.Monad.Operational.Mocks
import Data.Typeable
import Test.HUnit.Lang
import Test.Hspec
import Prelude hiding (getLine)

spec :: Spec
spec = do
  describe "viewMock" $ do
    it "converts single mocked commands" $ do
      let mock = do
            GetLine `returns` "foo"
      case mock of
        Cons (CommandCheck (Just "GetLine") _) _ (Nil ()) -> return ()
        _ -> fail "error" :: IO ()

    it "converts single return statements" $ do
      let viewed = do
            return "foo"
      case viewed of
        Nil "foo" -> return ()
        _ -> fail "error" :: IO ()

    it "converts multiple commands" $ do
      let viewed = do
            GetLine `returns` "foo"
            WriteLine "bar" `returns` ()
            GetLine `returns` "baz"
      case viewed of
        Cons (CommandCheck (Just "GetLine") _) _ (Cons (CommandCheck (Just "WriteLine") _) _ _) -> return ()
        _ -> fail "error" :: IO ()

    it "normalizes binds" $ do
      let viewed = do
            (GetLine `returns` "foo" >> WriteLine "bar" `returns` ())
            GetLine `returns` "baz"
      case viewed of
        Cons (CommandCheck (Just "GetLine") _) _ (Cons (CommandCheck (Just "WriteLine") _) _ _) -> return ()
        _ -> fail "error" :: IO ()

  describe "testWithMock" $ do
    it "allows to test against a sequence of command operations" $ do
      testWithMock lineReverse $ do
        GetLine `returns` "foo"
        WriteLine "oof" `returns` ()

    it "catches unexpected command calls" $ do
      let test =
            testWithMock lineReverse $ do
              GetLine `returns` "foo"
              GetLine `returns` "foo"
              return ()
      test `shouldError` "expected: call to GetLine, got: WriteLine"

    it "catches unexpected superfluous calls to commands" $ do
      let test = testWithMock lineReverse $ do
            GetLine `returns` "foo"
            return ()
      test `shouldError` "expected: function returns, got: WriteLine"

    it "catches missing calls to commands" $ do
      let test =
            testWithMock lineReverse $ do
              GetLine `returns` "foo"
              WriteLine "oof" `returns` ()
              GetLine `returns` "bar"
              return ()
      test `shouldError` "expected: call to GetLine, got: function returns"

    it "catches wrong results" $ do
      let test =
            testWithMock (lineReverse >> return True) $ do
              GetLine `returns` "foo"
              WriteLine "oof" `returns` ()
              return False
      HUnitFailure _ e :: HUnitFailure <- expectError test
      formatFailureReason e `shouldBe` "expected: False\n but got: True"

    it "has a Functor implementation" $ do
      testWithMock lineReverse $ do
        expected <- fmap reverse (GetLine `returns` "foo" >> return "foo")
        WriteLine expected `returns` ()

    it "has an Applicative implementation" $ do
      let testProgram = do
            output <- liftA2 (<>) getLine getLine
            writeLine output
      testWithMock testProgram $ do
        result <-
          (GetLine `returns` "foo" >> return reverse)
            <*> (GetLine `returns` "bar" >> return "bar")
        WriteLine ("foo" <> reverse result) `returns` ()

    it "allows to use predicates for arguments given to commands" $ do
      let predicate :: TestCommand other -> IO (Either () (() :~: other))
          predicate = \case
            WriteLine line -> do
              length line `shouldBe` 3
              return $ Right Refl
            _ -> return $ Left ()
      testWithMock lineReverse $ do
        GetLine `returns` "foo"
        commandSatisfies predicate ()
      let failing = testWithMock lineReverse $ do
            commandSatisfies predicate ()
      failing `shouldError` "predicate failed on: GetLine"
      let failing = testWithMock lineReverse $ do
            GetLine `returns` "foo"
            WriteLine "oof" `returns` ()
            commandSatisfies predicate ()
      failing `shouldError` "expected: call to another command, got: function returns"

    it "allows to test a command like Fork" $ do
      let forkedOutput :: TestProgram ()
          forkedOutput = do
            fork $ do
              writeLine "forked"
            writeLine "not forked"
      let checkForkedProgram :: TestCommand a -> IO (Either () (() :~: a))
          checkForkedProgram = \case
            Fork forked -> do
              testWithMock forked $ do
                WriteLine "forked" `returns` ()
              return $ Right Refl
            _ -> throwIO $ ErrorCall "expected: Fork"
      testWithMock forkedOutput $ do
        commandSatisfies checkForkedProgram ()
        WriteLine "not forked" `returns` ()

-- * commands

type TestProgram = Program TestCommand

data TestCommand a where
  GetLine :: TestCommand String
  WriteLine :: String -> TestCommand ()
  Fork :: TestProgram () -> TestCommand ()

getLine :: TestProgram String
getLine = singleton GetLine

writeLine :: String -> TestProgram ()
writeLine = singleton . WriteLine

fork :: TestProgram () -> TestProgram ()
fork = singleton . Fork

instance CommandEq TestCommand where
  commandEq GetLine GetLine = return $ Right Refl
  commandEq (WriteLine a) (WriteLine b) = do
    a `shouldBe` b
    return $ Right Refl
  commandEq _ _ = return $ Left ()

  showConstructor = \case
    GetLine -> "GetLine"
    WriteLine _ -> "WriteLine"

-- * test programs

lineReverse :: TestProgram ()
lineReverse = do
  l <- getLine
  writeLine (reverse l)

-- * test helpers

shouldError :: IO () -> String -> IO ()
shouldError test message = do
  ErrorCall m <- expectError test
  m `shouldBe` message

expectError :: (Exception e) => IO () -> IO e
expectError test =
  (test >> expectationFailure "expectError: no exception thrown" >> error "impossible")
    `catch` return
