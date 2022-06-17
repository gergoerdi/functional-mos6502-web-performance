HTML_FILES	= index.html base64.js main.js

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

