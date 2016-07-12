{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}

module Control.Monad.Operational.Mocks where

import           Control.Monad
import           Control.Monad.Operational
import           Data.Functor
import           Data.Text (Text)
import           Data.Type.Equality
import           Data.Typeable
import           Prelude hiding (gcd, log)
import           Test.Hspec
import           Unsafe.Coerce

run :: Program Prim a -> IO a
run m = case view m of
  Return a -> return a
  a :>>= b -> (>>= run . b) $ case a of
    GetLine -> getLine
    WriteLine s -> putStrLn s

type PP = Program Prim

data Prim a where
  GetLine :: Prim String
  WriteLine :: String -> Prim ()
  Log :: (Typeable l, Show l, Eq l) => l -> Prim ()
  Foo :: (Typeable a, Show a, Eq a) => a -> Prim a

deriving instance Show (Prim a)

instance CommandEq Prim where
  commandEq a b = case (a, b) of
    (GetLine, GetLine) -> Right Refl
    (WriteLine a, WriteLine b) -> eitherEq a b $> Refl
    (Log a, Log b) -> eitherEq a b $> Refl
    (Foo a, Foo b) -> eitherEq a b
    _ -> Left ()

wl :: String -> PP ()
wl s = singleton (WriteLine s)

gl :: PP String
gl = singleton GetLine

log :: (Typeable a, Eq a, Show a) => a -> PP ()
log a = singleton (Log a)

foo :: (Typeable a, Eq a, Show a) => a -> PP a
foo a = singleton (Foo a)

log' :: (Integer, Integer) -> Prim ()
log' = Log

-- * testing

data Mock prim a where
  (:>=) :: Show a => prim a -> a -> Mock prim b -> Mock prim b
  RReturn :: Show a => a -> Mock prim a

test :: (Show a, Eq a) => Program Prim a -> Mock Prim a -> IO ()
test program mock = case (mock, view program) of
  (((:>=) mockCommand mockNextArg nextMock), programCommand :>>= nextProgram) -> do
    case commandEq mockCommand programCommand of
      Left () -> error "commands not equal"
      Right refl -> do
        putStrLn $ "matched " ++ show mockCommand ++ " -> " ++ show mockNextArg
        test (nextProgram (castWith refl mockNextArg)) nextMock
  (RReturn m, Return p) ->
    m `shouldBe` p
  (RReturn x, _) -> error $ "expected: RReturn " ++ show x ++ ", got: bind"
  (_, Return _) -> error $ "expected: bind, got: return"

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  it "rev" $ do
    test rev $
      GetLine :>= "foo" $
      WriteLine "oof" :>= () $
      RReturn ()

  it "rev" $ do
    (test rev $
      GetLine :>= "foo" $
      GetLine :>= "foo" $
      WriteLine "oof" :>= () $
      RReturn ()) `shouldThrow` errorCall "commands not equal"

  it "gcd" $ do
    test (gcd 12 20) $
      log' (12, 20) :>= () $
      log' (12, 8) :>= () $
      log' (8, 4) :>= () $
      log' (4, 4) :>= () $
      WriteLine "result: 4" :>= () $
      RReturn 4

  it "log foo" $ do
    (test (log ("foo" :: String)) $
      Log ("foo" :: Text) :>= () $
      RReturn ()) `shouldThrow` errorCall "commands not equal"

  it "foo" $ do
    test (foo ()) $
      Foo () :>= () $
      RReturn ()

rev :: Program Prim ()
rev = do
  s <- gl
  wl $ reverse s

-- * gcd

gcd :: Integer -> Integer -> PP Integer
gcd a b = do
  log (a, b)
  if
    | a == b -> do
        wl ("result: " ++ show a)
        return a
    | a > b -> gcd b (a - b)
    | b > a -> gcd a (b - a)

-- * CommandEq

class CommandEq command where
  commandEq :: command a -> command b -> Either () (a :~: b)

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
