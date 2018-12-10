module Test.Main2 where

import Prelude
import Data.Generic.Rep (class Generic)
import Data.Debug (class Debug, debug, genericDebug, prettyPrint)
import Data.Map (Map)
import Data.Tuple (Tuple(..))
import Data.Map as Map
import Effect (Effect)
import Effect.Console (log)

data MyType a
  = A Int a
  | B (Array Int)
  | C (Map String a)

derive instance genericMyType :: Generic (MyType a) _

instance debugMyType :: Debug a => Debug (MyType a) where
  debug = genericDebug

main :: Effect Unit
main = do
  let
    p :: forall a. Debug a => a -> Effect Unit
    p = log <<< prettyPrint <<< debug

  p (A 3 false)
  p (B [10,15,3] :: MyType Void)
  p (C (Map.fromFoldable [Tuple "a" 3, Tuple "b" 6]))
