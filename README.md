# purescript-debugged

This is an experimental library, which attempts to provide an alternative to
the `Show` type class with a richer representation. This should provide a few
major benefits:

- Allows better display in a repl, e.g. we have the option of not attempting to
  display too much of a structure at once. A browser-based repl could possibly
  even allow users to interactively explore large and complex values.
- Provides the ability to diff expected vs actual structures in tests without
  having to write custom diffing logic for each new data type.
- Instead of returning a `String`, the class returns an opaque type, so there's
  less temptation to abuse the class for e.g. serialization.
- No expectation that the user should be able to "uneval" the result to produce
  the same value again; by giving the class a smaller and more clearly-defined
  purpose, it should hopefully help us to write uncontroversial instances for
  more types.

The intention (if this experiment turns out to be successful) is that every
data type of kind `Type` should have a `Debug` instance. This makes the repl's
UX a lot nicer, and is an important step towards banishing the dreaded
NoInstanceFound errors you often see when you're just trying to play with
something in the repl.

For more background, see my [Down with Show][] blog series.

Previous discussion:

- https://github.com/purescript/purescript/issues/1675
- https://github.com/purescript/purescript/issues/2731

## Examples

Define a data type:

```purescript
data MyType a
  = A Int a
  | B (a -> Int) (Array Int)
  | C (Map String a) (Map a String)
```

Derive some instances:

```purescript
derive instance genericMyType :: Generic (MyType a) _

instance debugMyType :: Debug a => Debug (MyType a) where
  debug = genericDebug
```

Now it's printable in the repl!

```purescript
> B identity [1,2,3]
B <function> [ 1, 2, 3 ]

> A 1 (A 2 (A 3 unit))
A 1 (A 2 (A 3 unit))
```

Not only that, but we can also diff structures and pretty-print the
differences:

```purescript
> items = [Tuple "a" 1, Tuple "b" 2, Tuple "c" 3]
> x = C (Map.fromFoldable items) (Map.fromFoldable (map swap items))
> y = C (Map.fromFoldable items) (Map.fromFoldable [Tuple 1 "aa", Tuple 2 "b", Tuple 3 "x"])
> log $ prettyPrintDelta $ diff x y
C
  <Map { "a": 1, "b": 2, "c": 3 }>
  <Map
  { 1: -"a" +"aa",
    2: "b",
    3: -"c" +"x" }>
```

and in your terminal, additions and deletions are highlighted green and red
respectively. Note that all you need for the above is a `Debug` instance!

### Some more pretty-printing examples

```purescript
module Test.Main where

import Prelude

import Data.Array (range)
import Data.Debug (class Debug, debug, diff, genericDebug, prettyPrintDelta)
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.List as L
import Data.List.Lazy as LL
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Console (log)
import PSCI.Support (eval)

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

-- note: the type signature is needed here for instance selection
eg :: forall a. Tuple (a -> a) (Tuple (Either Void (Maybe Unit)) (Either (Either Int Int) Int))
eg = Tuple identity (Tuple (Right Nothing) (Left (Left 3)))
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
[ unit ]
[ [ [ [ unit ] ] ] ]
[ [ 1, 2, 3 ],
  [ 4, 5, 6 ],
  [ 7, 8, 9 ] ]
[ Tuple "a" 1, Tuple "b" 2 ]
Tuple <function> (Tuple (Right Nothing) (Left (Left 3)))
<Repr
  value: Tuple <function> (Tuple (Right Nothing) (Left (...)))>
{ bar: "hi", foo: 1 }
{ bar: "hi",
  baz: { aah: Tuple "AAH" "AAAAH", quux: 3 },
  foo: 1 }
<Map { "a": 1, "b": 2 }>
<List
  [ 1,
    2,
    3,
    4,
    5,
    6,
    7,
    8,
    9,
    10 ]>
<List.Lazy
  [ 1,
    2,
    3,
    4,
    5,
    6,
    7,
    8,
    9,
    10 ]>
unit
None
PairA 3 3
Loads [ 1, 2, 3 ] (Right [ "hi" ])
```

## Warning!

This package currently contains a big hack to make it work with the repl. As of
PureScript v0.12.1, there is no way to configure the function used by the repl
for printing the results of evaluating expressions. Instead, the repl is
hardcoded to use the function `eval` from the module `PSCI.Support`. Therefore,
in order to allow the repl to use `Debug` instances for printing results, we
need to define our own `PSCI.Support` module.  This is a little problematic,
because if a package depends on both `purescript-debugged` and
`purescript-psci-support`, we will see a module name clash.

There is [a compiler issue (#3177)](https://github.com/purescript/purescript/issues/3177)
for addressing this, but for now, this package is only usable in projects which
do not depend on `purescript-psci-support`.

## License

This code is MIT licensed; the `Debug (Record a)` instance is adapted from
@matthewleon's [purescript-record-show][] library.

[purescript-record-show]: https://github.com/matthewleon/purescript-record-show
[Down with Show]: https://harry.garrood.me/blog/down-with-show-part-1/
