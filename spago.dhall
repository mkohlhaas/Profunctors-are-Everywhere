{ name = "my-project"
, dependencies =
  [ "console", "effect", "maybe", "numbers", "prelude", "strings", "tuples" ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
