{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE InstanceSigs #-}

module Control.Monad.Free.MocksSpec where

import Control.Exception hiding (assert)
import Data.Functor
import Test.Hspec
import Data.Maybe
import Unsafe.Coerce
import Control.Monad.Free
import Data.Type.Equality
import Control.Monad.Trans.State
import Control.Monad.IO.Class
import Debug.Trace
import Prelude hiding (getLine)

-- * Functor
data ConsoleF a where
  GetLine :: (String -> a) -> ConsoleF a
  WriteLine :: String -> a -> ConsoleF a

deriving instance Functor ConsoleF

instance Show (ConsoleF a) where
  show (GetLine _) = "GetLine"
  show (WriteLine _ _) = "WriteLine"

type Console = Free ConsoleF

-- instance ShowConstructor ConsoleF where
--   showConstructor (GetLine _)  = "GetLine"
--   showConstructor (WriteLine _ _) = "WriteLine"

class CommandEq1 f where
  eqC :: f a -> f b  -> Maybe ()

class ShowConstructor f where
  showConstructor :: f a -> String

instance ShowConstructor ConsoleF where
  showConstructor (GetLine _) = "GetLine"
  showConstructor (WriteLine _ _) = "WriteLine"

instance CommandEq1 ConsoleF where
  eqC (GetLine _) (GetLine _) = Just ()
  eqC (WriteLine s _) (WriteLine s2 _) = if s == s2 then Just () else Nothing
  eqC _ _ = Nothing

-- instance CommandEq ConsoleF where
--   -- <fixme>
--   -- commandEq :: ConsoleF a -> ConsoleF b -> IO (Either () (a :~: b))
--   -- commandEq (GetLine _) (GetLine _) = pure $ Right Refl
--   -- commandEq (WriteLine s _) (WriteLine s1 _) = (s `shouldBe` s1) *> pure (Right Refl)
--   commandEq _ _ = pure (Left ())

-- * Primitive ops
getLineF = GetLine id

writeLineF s = WriteLine s ()

getLine :: Console String
getLine = liftF getLineF

writeLine :: String -> Console ()
writeLine = liftF . writeLineF

-- * programs for testing
getAndWrite' :: Console ()
getAndWrite' =
  getLine >>=
  writeLine . reverse

mock :: Mock f-> Mock f
mock = id

testFreeWithMock :: ShowConstructor f => Free f a -> Mock f -> IO ()
testFreeWithMock freeTerms mockTerms = runStateT (foldFree with freeTerms) mockTerms >>= assertPure
  where
    with currentTerm = do
          liftIO $ print (showConstructor currentTerm)
          mocks <- get
          case mocks of
            [] ->  liftIO $ throwIO $ ErrorCall $ "expected: function returns, got: " ++ showConstructor currentTerm
            (mock : rest) -> do
              put rest
              liftIO (assert currentTerm mock)

    assertPure (_, []) = pure ()
    assertPure (_, _) = liftIO $ throwIO $ ErrorCall $
      "expected: call to a primitive" ++
      ", got: function returns"



type Mock f = [Mocking f]
data Mocking f = Mocking (forall r . (forall a . (CommandEq1 f) => f a -> a -> r) -> r)

returns :: CommandEq1 f => f a -> a -> Mocking f
returns term result = Mocking (\f -> f term result)

assert :: ShowConstructor f => f a -> Mocking f -> IO a
assert fa (Mocking f) = f (\mocked returnVal ->
                             if isJust (eqC fa mocked)
                             then pure $ unsafeCoerce returnVal --TODO, remove unsafecoerce
                             else throwIO $ ErrorCall $
                                  "expected: call to " ++ showConstructor mocked ++
                                  ", got: " ++ showConstructor fa )

andThen :: Mocking f -> Mock f -> Mock f
andThen = (:)

infixr 8 `andThen`

spec :: Spec
spec = describe "Free Mocks Specs" $
  it "should allow to mock a sequence of operations" $ do
    foldFree (\x -> case x of
                 s@(GetLine f) -> putStrLn (showConstructor s) *> pure (f "Testing")
                 p@(WriteLine s f) -> putStrLn (showConstructor p) *> putStr s *> pure f
                 ) getAndWrite'

    let mockConsole = mock $
          getLineF `returns` "Some string" `andThen`
          writeLineF "Some string" `returns` () `andThen`
          []

    testFreeWithMock getAndWrite' mockConsole
