{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}

module Control.Monad.Free.Mocks where

import Control.Exception hiding (assert)
import Data.Maybe
import Unsafe.Coerce
import Control.Monad.Free
import Control.Monad.Trans.State
import Control.Monad.IO.Class
import Prelude hiding (getLine)

mock :: Mock f -> Mock f
mock = id

class Assert f where
  assert :: f a -> f b -> IO (Maybe ())

class ShowConstructor f where
  showConstructor :: f a -> String

class HasMock f where
  mockE :: f a -> Either a (WrappedStep a)

data WrappedStep a = forall s . W (s -> a)

type Mock f = [Mocking f]
data Mocking f = Mocking (forall r . (forall a . (HasMock f, Assert f) => f a -> a -> r) -> r)

testFreeWithMock :: ShowConstructor f => Free f a -> Mock f -> IO ()
testFreeWithMock freeTerms mockTerms = runStateT (foldFree with freeTerms) mockTerms >>= assertPure
  where
    with currentTerm = do
          mocks <- get
          case mocks of
            [] ->
              liftIO $ throwIO $ ErrorCall $
              "expected: function returns" ++
              ", got: " ++ showConstructor currentTerm
            (m : rest) -> do
              put rest
              liftIO (assertMock currentTerm m)

    assertPure (_, []) = pure ()
    assertPure (_, _) = (liftIO $ throwIO $ ErrorCall $
                          "expected: call to a primitive" ++
                          ", got: function returns")

returns :: (Assert f, HasMock f) => f a -> a -> Mocking f
returns term result = Mocking (\f -> f term result)

assertMock :: ShowConstructor f => f a -> Mocking f -> IO a
assertMock fa (Mocking f) = f (\mocked returnVal -> do
                             r <- assert fa mocked
                             if isJust r
                             then pure $ case mockE fa of
                               Right (W next) -> next $ unsafeCoerce returnVal
                               Left a -> a
                             else throwIO $ ErrorCall $
                                  "expected: call to " ++ showConstructor mocked ++
                                  ", got: " ++ showConstructor fa )

andThen :: Mocking f -> Mock f -> Mock f
andThen = (:)

infixr 8 `andThen`
