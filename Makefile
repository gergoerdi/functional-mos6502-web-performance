HTML_FILES	= index.html base64.js main.js

.ONESHELL:

all: $(patsubst %, _build/%, $(HTML_FILES)) _build/files.js

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
