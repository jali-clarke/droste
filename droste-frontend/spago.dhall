{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "droste-frontend"
, dependencies = [
  "affjax"
  , "foreign-generic"
  , "psci-support"
  , "react"
  , "react-dom"
  , "web-html"
]
, packages = ./packages.dhall
, sources = ["src/**/*.purs"]
}
