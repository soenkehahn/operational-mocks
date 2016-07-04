{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE StandaloneDeriving #-}

import           Control.Monad.Operational
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
  Log :: Show a => a -> Prim ()

deriving instance Show (Prim a)

wl :: String -> PP ()
wl s = singleton (WriteLine s)

gl :: PP String
gl = singleton GetLine

log :: Show a => a -> PP ()
log a = singleton (Log a)

-- * testing

data Mock prim a where
  (:>=) :: Show a => prim a -> a -> Mock prim b -> Mock prim b
  RReturn :: Show a => a -> Mock prim a

test :: (Show a, Eq a) => PP a -> Mock Prim a -> IO ()
test program mock = case (mock, view program) of
  (((:>=) mockCommand mockNextArg nextMock), programCommand :>>= nextProgram) -> do
    show programCommand `shouldBe` show mockCommand
    putStrLn $ "matched " ++ show mockCommand ++ " -> " ++ show mockNextArg
    test (nextProgram (unsafeCoerce mockNextArg)) nextMock
  (RReturn m, Return p) ->
    m `shouldBe` p
  (RReturn x, _) -> error $ "expected: RReturn " ++ show x ++ ", got: bind"
  (_, Return _) -> error $ "expected: bind, got: return"

main :: IO ()
main = do
  test rev $
    GetLine :>= "foo" $
    WriteLine "oof" :>= () $
    RReturn ()
  test (gcd 12 20) $
    Log (12, 20) :>= () $
    Log (12, 8) :>= () $
    Log (8, 4) :>= () $
    Log (4, 4) :>= () $
    WriteLine "result: 4" :>= () $
    RReturn 4

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
