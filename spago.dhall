{ name = "debugged"
, license = "MIT"
, repository = "https://github.com/hdgarrood/purescript-debugged"
, dependencies =
  [ "arrays"
  , "bifunctors"
  , "console"
  , "datetime"
  , "effect"
  , "either"
  , "enums"
  , "exceptions"
  , "generics-rep"
  , "lists"
  , "ordered-collections"
  , "prelude"
  , "psci-support"
  , "record"
  , "strings"
  , "tuples"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
