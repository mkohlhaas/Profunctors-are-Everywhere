{ name = "my-project"
, dependencies =
  [ "console"
  , "effect"
  , "either"
  , "foldable-traversable"
  , "identity"
  , "maybe"
  , "newtype"
  , "numbers"
  , "prelude"
  , "strings"
  , "tuples"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
