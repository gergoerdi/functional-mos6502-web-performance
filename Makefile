HTML_FILES	= index.html base64.js main.js
IMPLS		= ghcjs/main.js ghcjs/wrap.js js/mos6502.js
# IMPLS		= purescript/bundle.js idris2/main.js js/mos6502.js

.ONESHELL:

all: $(patsubst %, _build/%, $(HTML_FILES)) _build/files.js $(patsubst %,_build/implementations/%, $(IMPLS))

_build/index.html: html/index.html
	mkdir -p _build
	cp -f $< $@

_build/base64.js: html/base64.js
	mkdir -p _build
	cp -f $< $@

_build/main.js: html/main.js
	mkdir -p _build
	cp -f $< $@

_build/files.js: data/program.dat
	mkdir -p _build
	(echo "let files = {"; \
	$(foreach file, $<, \
	        printf "\t'%s': base64ToArrayBuffer('" $(file) ; \
	        base64 $(file) | sed -e 's/$$/\\/' ; \
	        printf "'),\n" ; \
	        ) \
	echo "};"; \
	) > $@

_build/implementations/purescript/bundle.js:
	mkdir -p $(dir $@)
	cd implementations/purescript
	[ $$(node -p "require('big-integer/package.json').version") != "1.6.51" ] && \
		npm install big-integer@1.6.51
	spago bundle-module -t ../../$@

implementations/idris2/build/exec/main.js:
	cd implementations/idris2
	idris2 --build fp-perf-mos6502-idris2.ipkg

_build/implementations/idris2/main.js: implementations/idris2/build/exec/main.js
	mkdir -p $(dir $@)
	cp -f $< $@

_build/implementations/js/mos6502.js: implementations/js/mos6502.js
	mkdir -p $(dir $@)
	cp -f $< $@

_build/implementations/ghcjs/wrap.js: implementations/ghcjs/wrap.js
	mkdir -p $(dir $@)
	cp -f $< $@

_build/implementations/ghcjs/main.js: implementations/ghcjs/dist-newstyle/build/js-ghcjs/ghcjs-8.10.7/fp-perf-mos6502-asterius-0.1.0/x/main/build/main/main.jsexe/all.js
	mkdir -p $(dir $@)
	cp -f $< $@

implementations/asterius/_build/Driver.wasm:
	cd implementations/asterius
	mkdir -p _build
	docker run -it --rm -v $(shell readlink -f implementations/asterius):/workspace -w /workspace terrorjack/asterius \
	  ahc-link --browser \
	    --input-hs src/Driver.hs \
	    --ghc-option "-O2" \
	    --export-function run \
	    --input-mjs index.js --no-main \
	    --output-directory _build


_build/Driver.wasm: implementations/asterius/_build/Driver.wasm
	mkdir -p $(dir $@)
	cp -f $< $@
