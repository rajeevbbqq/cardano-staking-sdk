{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name =
    "text-encoding"
, dependencies =
    [ "arraybuffer-types"
    , "console"
    , "effect"
    , "either"
    , "exceptions"
    , "functions"
    , "partial"
    , "prelude"
    , "psci-support"
    , "strings"
    , "strongcheck"
    , "unicode"
    ]
, packages =
    ./packages.dhall
, sources =
    [ "src/**/*.purs", "test/**/*.purs" ]
}
