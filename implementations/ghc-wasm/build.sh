mkdir -p _build

~/prog/haskell/ghc/ghc-wasm/_build/stage1/bin/wasm32-wasi-ghc -O2 -isrc src/main.hs  -outputdir _build/ -o _build/main.wasm -optl-Wl,--export=run,--export=newBuffer,--export=hs_init -optl-mexec-model=reactor
