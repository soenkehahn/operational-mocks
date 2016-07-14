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
        Result ()

    it "catches unexpected primitive calls" $ do
      let test = testWithMock lineReverse $
            GetLine `returns` "foo" `andThen`
            GetLine `returns` "foo" `andThen`
            Result ()
      test `shouldThrow` errorCall "expected: call to GetLine, got: WriteLine"

    it "allows to test a primitive like Fork" $ do
      let forkedOutputMock :: TestPrim a -> IO (a :~: ())
          forkedOutputMock = \ case
            Fork forked -> do
              let mock = (WriteLine "forked" `returns` () `andThen` Result ())
              testWithMock forked mock
              return Refl
            _ -> throwIO $ ErrorCall "expected: Fork"
      testWithMock forkedOutput $
        testPrimitive forkedOutputMock () `andThen`
        WriteLine "not forked" `returns` () `andThen`
        Result ()

-- * primitives

type TestProgram = Program TestPrim

data TestPrim a where
  GetLine :: TestPrim String
  WriteLine :: String -> TestPrim ()
  Fork :: TestProgram () -> TestPrim ()

  ForkMock :: Mock TestPrim () -> TestPrim ()

getLine :: TestProgram String
getLine = singleton GetLine

writeLine :: String -> TestProgram ()
writeLine = singleton . WriteLine

fork :: TestProgram () -> TestProgram ()
fork = singleton . Fork

instance CommandEq TestPrim where
  commandEq GetLine GetLine = return $ Right Refl
  commandEq (WriteLine a) (WriteLine b) = do
    a `shouldBe` b
    return $ Right Refl
  commandEq (Fork forked) (ForkMock mockForked) = do
    testWithMock forked mockForked
    return $ Right Refl
  commandEq _ _ = return $ Left ()

  showConstructor = \ case
    GetLine -> "GetLine"
    WriteLine _ -> "WriteLine"
    Fork _ -> "Fork"
    ForkMock _ -> "ForkMock"

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
