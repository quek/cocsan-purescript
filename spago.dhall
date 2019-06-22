{ sources =
    [ "src/**/*.purs", "test/**/*.purs" ]
, name =
    "coc"
, packages =
    ./packages.dhall
, dependencies =
    [ "prelude"
    , "console"
    , "effect"
    , "variant"
    , "nonempty"
    , "aff"
    , "halogen"
    , "halogen-formless"
    , "remotedata"
    , "routing"
    , "formatters"
    , "now"
    , "affjax"
    , "slug"
    , "precise-datetime"
    , "typelevel-prelude"
    , "argonaut-core"
    , "argonaut-codecs"
    , "aff-bus"
    , "aff-promise"
    , "aff-coroutines"
    , "foreign-generic"
    , "psci-support"
    ]
}
