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

## License

This code is MIT licensed; the `Debug (Record a)` instance is adapted from
@matthewleon's [purescript-record-show][] library.

[purescript-record-show]: https://github.com/matthewleon/purescript-record-show
