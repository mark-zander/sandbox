{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "my-project"
, dependencies =
  [ "array-views"
  , "arrays"
  , "console"
  , "effect"
  , "exists"
  , "maybe"
  , "psci-support"
  , "record"
  , "strings"
  , "test-unit"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
