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
  , "math"
  , "maybe"
  , "psci-support"
  , "quickcheck-laws"
  , "record"
  , "strings"
  , "test-unit"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
