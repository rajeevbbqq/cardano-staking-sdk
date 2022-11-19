{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "singularitynet-frontend"
, dependencies =
  [ "aff"
  , "aff-promise"
  , "argonaut-codecs"
  , "argonaut-core"
  , "arrays"
  , "bifunctors"
  , "bigints"
  , "cardano-transaction-lib"
  , "console"
  , "control"
  , "datetime"
  , "effect"
  , "exceptions"
  , "integers"
  , "math"
  , "maybe"
  , "monad-logger"
  , "now"
  , "ordered-collections"
  , "partial"
  , "prelude"
  , "record"
  , "strings"
  , "transformers"
  , "uint"
  , "unfoldable"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "exe/**/*.purs", "test/**/*.purs" ]
}
