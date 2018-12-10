module Test.Main where

import Prelude

import Data.Array (range)
import Data.Debug (class Debug, debug, genericDebug, diffed, prettyPrintDelta)
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.List as L
import Data.List.Lazy as LL
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Console (log)
import Effect.Exception (throw)
import PSCI.Support (eval)

superbAssertEqual :: forall a. Eq a => Debug a => a -> a -> Effect Unit
superbAssertEqual x y =
  if x == y
    then pure unit
    else do
       log (prettyPrintDelta (diffed x y))
       throw "Test failed"

data Example a b
  = None
  | PairA a a
  | PairB b b
  | Loads (Array a) (Either a b)

derive instance genericExample :: Generic (Example a b) _
derive instance eqExample :: (Eq a, Eq b) => Eq (Example a b)

type Eg = Example Int (Array String)

instance debugExample :: (Debug a, Debug b) => Debug (Example a b) where
  debug = genericDebug

main :: Effect Unit
main = do
  let p = eval

  p 24
  p 1.4e10
  p 1.4e+30
  p 0
  p '\300'
  p (-3)
  p (-3.0)
  p (Tuple 1 (-1))
  p (Tuple 1.0 (-1.0))
  p unit
  p [unit]
  p [[[[unit]]]]
  p [[1,2,3], [4,5,6], [7,8,9] ]
  p [Tuple "a" 1, Tuple "b" 2]
  p eg
  p (debug eg)
  p {foo: 1, bar: "hi"}
  p {foo: 1, bar: "hi", baz: {quux: 3, aah: Tuple "AAH" "AAAAH"}}
  p (Map.fromFoldable [Tuple "a" 1, Tuple "b" 2])
  p (L.fromFoldable (range 1 10))
  p (LL.fromFoldable (range 1 10))
  p (pure unit :: Effect Unit)

  p (None :: Eg)
  p (PairA 3 3 :: Eg)
  p (Loads [1,2,3] (Right ["hi"]))

  let
    x = Loads [1,2,3,4,5] (Right ["hi", "world"])
    y = Loads [1,2,4,3,5] (Right [])

  superbAssertEqual x y

-- note: the type signature is needed here for instance selection
eg :: forall a. Tuple (a -> a) (Tuple (Either Void (Maybe Unit)) (Either (Either Int Int) Int))
eg = Tuple identity (Tuple (Right Nothing) (Left (Left 3)))

