{ name = "fp-perf-mos6502-purs"
, dependencies =
  [ "arraybuffer"
  , "arraybuffer-types"
  , "arrays"
  , "effect"
  , "integers"
  , "maybe"
  , "numerics"
  , "partial"
  , "prelude"
  , "profunctor-lenses"
  , "refs"
  , "strings"
  , "tailrec"
  , "transformers"
  , "uint"
  , "uncurried-transformers"
  , "word"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
