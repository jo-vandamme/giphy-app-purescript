{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "my-project"
, dependencies =
  [ "affjax"
  , "argonaut-core"
  , "codec-argonaut"
  , "console"
  , "css"
  , "effect"
  , "formatters"
  , "halogen"
  , "halogen-css"
  , "numbers"
  , "psci-support"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
