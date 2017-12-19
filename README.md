# purescript-debugged

This is an experimental library, which attempts to provide an alternative to
the `Show` type class with a richer representation. This should hopefully
provide a few benefits:

- Not as convenient as `String`, so there's no temptation to abuse the class
  for serialization, unlike `Show`.
- Allows better display in a REPL, e.g. we have the option of not attempting to
  display too much of a structure at once. A future browser-based REPL could
  possibly even allow users to interactively explore large and complex values.
- Provides the ability to diff expected vs actual structures in tests without
  having to write custom diffing logic for each new data type.
- No expectation that the user should be able to "uneval" the result to produce
  the same value again, i.e. by giving the class a smaller and more
  clearly-defined purpose, it should hopefully help us to write uncontroversial
  instances for more types.

The intention (if this experiment turns out to be successful) is that every
data type of kind `Type` should have a `Debug` instance, so that you never get
the dreaded NoInstanceFound error when you're just trying to see something in
the repl.

Previous discussion:

- https://github.com/purescript/purescript/issues/1675
- https://github.com/purescript/purescript/issues/2731

## Examples

Input:

```purescript
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
import Control.Monad.Eff (Eff)
import Data.Debugged (class Debug, print', debugged)

main = do
  let p :: forall a. Debug a => a -> _
      p x = print' x

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
  p (debugged eg)
  p {foo: 1, bar: "hi"}
  p {foo: 1, bar: "hi", baz: {quux: 3, aah: Tuple "AAH" "AAAAH"}}
  p (Map.fromFoldable [Tuple "a" 1, Tuple "b" 2])
  p (L.fromFoldable (range 1 10))
  p (LL.fromFoldable (range 1 10))
  p (pure unit :: Eff _ Unit)

-- note: the type signature is needed here for instance selection
eg :: forall a. Tuple (a -> a) (Tuple (Either Void (Maybe Unit)) (Either (Either Int Int) Int))
eg = Tuple id (Tuple (Right Nothing) (Left (Left 3)))
```

Output:

```
24
14000000000.0
1.4e+30
0
'Ĭ'
-3
-3.0
Tuple 1 (-1)
Tuple 1.0 (-1.0)
unit
[unit]
[[[[unit]]]]
[Tuple "a" 1, Tuple "b" 2]
Tuple <function> (Tuple (Right Nothing) (Left (Left 3)))
DExpr "Tuple" [DOpaque "function" [], DExpr "Tuple" [DExpr "Right" [DExpr "Nothing" []], DExpr "Left" [DExpr "Left" [DInt 3]]]]
{bar: "hi", foo: 1}
{bar: "hi", baz: {aah: Tuple "AAH" "AAAAH", quux: 3}, foo: 1}
<Map {"a": 1, "b": 2}>
<List [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]>
<List.Lazy [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]>
<Eff>
```


## Status

To be done:

- Diff `Debugged` expression trees
- Decide whether to hide `Debugged` constructors
- Pretty-print `Debugged` expression trees
- Provide `uneval :: Debugged -> Maybe String`?

## License

This code is MIT licensed; the `Debug (Record a)` instance is adapted from
@matthewleon's [purescript-record-show][] library.

[purescript-record-show]: https://github.com/matthewleon/purescript-record-show
