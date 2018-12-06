module Test.Main where

import Prelude
import Data.Tuple (Tuple(..))
import Data.Maybe (Maybe(..))
import Data.Either (Either(..))
import Data.List as L
import Data.List.Lazy as LL
import Data.Array (range)
import Data.Map (Map)
import Data.Map as Map
import Effect (Effect)
import Data.Debugged (class Debug, debugged)
import PSCI.Support (eval)

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
  p [Tuple "a" 1, Tuple "b" 2]
  p eg
  -- p (debugged eg)
  p {foo: 1, bar: "hi"}
  p {foo: 1, bar: "hi", baz: {quux: 3, aah: Tuple "AAH" "AAAAH"}}
  p (Map.fromFoldable [Tuple "a" 1, Tuple "b" 2])
  p (L.fromFoldable (range 1 10))
  p (LL.fromFoldable (range 1 10))
  p (pure unit :: Effect Unit)

-- note: the type signature is needed here for instance selection
eg :: forall a. Tuple (a -> a) (Tuple (Either Void (Maybe Unit)) (Either (Either Int Int) Int))
eg = Tuple identity (Tuple (Right Nothing) (Left (Left 3)))

