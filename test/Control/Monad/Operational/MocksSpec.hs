{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}

module Control.Monad.Operational.MocksSpec where

import           Control.Exception
import           Control.Monad.Operational
import           Data.Typeable
import           Prelude hiding (getLine)
import           Test.Hspec

import           Control.Monad.Operational.Mocks

spec :: Spec
spec = do
  describe "testWithMock" $ do
    it "allows to test against a sequence of primitive operations" $ do
      testWithMock lineReverse $
        GetLine `returns` "foo" `andThen`
        WriteLine "oof" `returns` () `andThen`
        result ()

    it "catches unexpected primitive calls" $ do
      let test = testWithMock lineReverse $
            GetLine `returns` "foo" `andThen`
            GetLine `returns` "foo" `andThen`
            result ()
      test `shouldThrow` errorCall "expected: call to GetLine, got: WriteLine"

    it "catches unexpected superfluous calls to primitives" $ do
      let test = testWithMock lineReverse $
            GetLine `returns` "foo" `andThen`
            result ()
      test `shouldThrow` errorCall "expected: function returns, got: WriteLine"

    it "allows to test a primitive like Fork" $ do
      let forkedOutputMock :: TestPrimitive a -> IO (a :~: ())
          forkedOutputMock = \ case
            Fork forked -> do
              let mock = (WriteLine "forked" `returns` () `andThen` result ())
              testWithMock forked mock
              return Refl
            _ -> throwIO $ ErrorCall "expected: Fork"
      testWithMock forkedOutput $
        testPrimitive forkedOutputMock () `andThen`
        WriteLine "not forked" `returns` () `andThen`
        result ()

-- * primitives

type TestProgram = Program TestPrimitive

data TestPrimitive a where
  GetLine :: TestPrimitive String
  WriteLine :: String -> TestPrimitive ()
  Fork :: TestProgram () -> TestPrimitive ()

getLine :: TestProgram String
getLine = singleton GetLine

writeLine :: String -> TestProgram ()
writeLine = singleton . WriteLine

fork :: TestProgram () -> TestProgram ()
fork = singleton . Fork

instance CommandEq TestPrimitive where
  commandEq GetLine GetLine = return $ Right Refl
  commandEq (WriteLine a) (WriteLine b) = do
    a `shouldBe` b
    return $ Right Refl
  commandEq (Fork _) (Fork _) = do
    error "can't test forks like this"
  commandEq _ _ = return $ Left ()

instance ShowConstructor TestPrimitive where
  showConstructor = \ case
    GetLine -> "GetLine"
    WriteLine _ -> "WriteLine"
    Fork _ -> "Fork"

-- * test programs

lineReverse :: TestProgram ()
lineReverse = do
  l <- getLine
  writeLine (reverse l)

forkedOutput :: TestProgram ()
forkedOutput = do
  fork $ do
    writeLine "forked"
  writeLine "not forked"
