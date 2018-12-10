module Test.Main2 where

import Prelude
import Data.Generic.Rep (class Generic)
import Data.Debug (class Debug, debug, genericDebug, prettyPrint, prettyPrintDelta, diffed)
import Data.Map (Map)
import Data.Tuple (Tuple(..), swap)
import Data.Map as Map
import Effect (Effect)
import Effect.Exception (throw)
import Effect.Console (log)

superbAssertEqual :: forall a. Eq a => Debug a => a -> a -> Effect Unit
superbAssertEqual x y =
  if x == y
    then pure unit
    else do
       log "Test failed:"
       log (prettyPrintDelta (diffed x y))

data MyType a
  = A Int a
  | B (Array Int)
  | C (Map String a) (Map a String)

derive instance eqMyType :: Eq a => Eq (MyType a)
derive instance genericMyType :: Generic (MyType a) _

instance debugMyType :: Debug a => Debug (MyType a) where
  debug = genericDebug

main :: Effect Unit
main = do
  let
    p :: forall a. Debug a => a -> Effect Unit
    p = log <<< prettyPrint <<< debug

  p (A 3 false)
  p (A 3 (A 4 true))
  p (B [10,15,3] :: MyType Void)

  let items = [Tuple "a" 3, Tuple "b" 6]
  p (C (Map.fromFoldable items) (Map.fromFoldable (map swap items)))

  let
    items = [Tuple "a" 3, Tuple "b" 6]
    x = C (Map.fromFoldable items) (Map.fromFoldable (map swap items))
    y = C (Map.fromFoldable items) (Map.fromFoldable (map swap items <> [Tuple 6 "c"]))

  superbAssertEqual x y
