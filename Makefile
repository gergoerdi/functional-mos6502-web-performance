HTML_FILES	= index.html base64.js main.js
IMPLS		= purescript/bundle.js idris2/main.js js/mos6502.js

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
