module Test.Main where

import Prelude

import Data.Array (range)
import Data.Debugged (class Debug, debugged, genericDebug)
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.List as L
import Data.List.Lazy as LL
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import PSCI.Support (eval)

data Example a b
  = None
  | PairA a a
  | PairB b b
  | Loads (Array a) (Either a b)

derive instance genericExample :: Generic (Example a b) _

type Eg = Example Int String

instance debugExample :: (Debug a, Debug b) => Debug (Example a b) where
  debugged = genericDebug

main = do
  let p :: forall a. Debug a => a -> _
      p x = eval x

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
  p (debugged eg)
  p {foo: 1, bar: "hi"}
  p {foo: 1, bar: "hi", baz: {quux: 3, aah: Tuple "AAH" "AAAAH"}}
  p (Map.fromFoldable [Tuple "a" 1, Tuple "b" 2])
  p (L.fromFoldable (range 1 10))
  p (LL.fromFoldable (range 1 10))
  p (pure unit :: Effect Unit)

  p (None :: Eg)
  p (PairA 3 3 :: Eg)
  p (Loads [1,2,3] (Right "hi"))

-- note: the type signature is needed here for instance selection
eg :: forall a. Tuple (a -> a) (Tuple (Either Void (Maybe Unit)) (Either (Either Int Int) Int))
eg = Tuple identity (Tuple (Right Nothing) (Left (Left 3)))

