let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.15.0-20220525/packages.dhall
        sha256:5facfdf9c35801a0e6a41b08b4293f947743007a9224a2a3d7694d87a44a7f28

in  upstream
  with word =
       { dependencies = 
         [ "prelude"
         , "numerics"
         , "strings"
         , "uint"
         , "bigints"
         ]
       , repo = "https://github.com/gergoerdi/purescript-word"
       , version = "v0.4.2"
       }
  with numerics =
       { dependencies =
         [ "prelude", "integers", "rationals", "uint", "bigints" ]
       , repo = "https://github.com/Proclivis/purescript-numerics"
       , version = "v0.1.2"
       }
