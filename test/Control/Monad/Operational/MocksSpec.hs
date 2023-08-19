{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Control.Monad.Operational.MocksSpec where

import Control.Exception (ErrorCall (ErrorCall), Exception, catch)
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
    it "converts single mocked primitives" $ do
      let mock = do
            GetLine `returns` "foo"
      case mock of
        Cons GetLine "foo" (Nil ()) -> return ()
        _ -> fail "error" :: IO ()

    it "converts single return statements" $ do
      let viewed = do
            return "foo"
      case viewed of
        Nil "foo" -> return ()
        _ -> fail "error" :: IO ()

    it "converts multiple primitives" $ do
      let viewed = do
            GetLine `returns` "foo"
            WriteLine "bar" `returns` ()
            GetLine `returns` "baz"
      case viewed of
        Cons GetLine "foo" (Cons (WriteLine "bar") _ _) -> return ()
        _ -> fail "error" :: IO ()

    it "normalizes binds" $ do
      let viewed = do
            (GetLine `returns` "foo" >> WriteLine "bar" `returns` ())
            GetLine `returns` "baz"
      case viewed of
        Cons GetLine "foo" (Cons (WriteLine "bar") _ _) -> return ()
        _ -> fail "error" :: IO ()

  describe "testWithMock" $ do
    it "allows to test against a sequence of primitive operations" $ do
      testWithMock lineReverse $ do
        GetLine `returns` "foo"
        WriteLine "oof" `returns` ()

    it "catches unexpected primitive calls" $ do
      let test =
            testWithMock lineReverse $ do
              GetLine `returns` "foo"
              GetLine `returns` "foo"
              return ()
      test `shouldError` "expected: call to GetLine, got: WriteLine"

    it "catches unexpected superfluous calls to primitives" $ do
      let test = testWithMock lineReverse $ do
            GetLine `returns` "foo"
            return ()
      test `shouldError` "expected: function returns, got: WriteLine"

    it "catches missing calls to primitives" $ do
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
            lines <- replicateM 2 getLine
            writeLine $ mconcat lines
      testWithMock testProgram $ do
        result <-
          (GetLine `returns` "foo" >> return reverse)
            <*> (GetLine `returns` "bar" >> return "bar")
        WriteLine ("foo" <> reverse result) `returns` ()

-- * primitives

type TestProgram = Program TestPrimitive

data TestPrimitive a where
  GetLine :: TestPrimitive String
  WriteLine :: String -> TestPrimitive ()

getLine :: TestProgram String
getLine = singleton GetLine

writeLine :: String -> TestProgram ()
writeLine = singleton . WriteLine

instance CommandEq TestPrimitive where
  commandEq GetLine GetLine = return $ Right Refl
  commandEq (WriteLine a) (WriteLine b) = do
    a `shouldBe` b
    return $ Right Refl
  commandEq _ _ = return $ Left ()

instance ShowConstructor TestPrimitive where
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
