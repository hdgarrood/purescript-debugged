module Test.Main where

import Prelude
import Data.Tuple (Tuple(..))
import Data.Maybe (Maybe(..))
import Data.Either (Either(..))
import Data.Map (Map)
import Data.Map as Map
import Data.Debugged (print, print')

main = do
  p 24
  p 1.4e17
  p 0
  p (-3)
  p (-3.0)
  p (Tuple 1 (-1))
  p (Tuple 1.0 (-1.0))
  p unit
  p [unit]
  p [[[[unit]]]]
  p [Tuple "a" 1, Tuple "b" 2]
  p eg
  p (Map.fromFoldable [Tuple "a" 1, Tuple "b" 2])

eg :: forall a. Tuple (a -> a) (Tuple (Either Void (Maybe Unit)) (Either (Either Int Int) Int))
eg = Tuple id (Tuple (Right Nothing) (Left (Left 3)))

p = print'
