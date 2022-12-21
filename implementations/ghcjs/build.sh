GHC="$HOME/prog/haskell/ghc/ghc/_build/ghc-stage1"

mkdir -p _build
$GHC --make -O -isrc -outputdir _build -o _build/main src/main.hs
