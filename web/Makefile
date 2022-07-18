ESBUILD := yarn esbuild

all:
	dune build live.bc.js

deploy: all
	mkdir -p $(WHERE)
	cp -v ../_build/default/web/*js index.html style.css $(WHERE)
	cp -v ../doc/logo/nuScr.png $(WHERE)
	chmod +w $(WHERE)/*.js

minify: deploy
	$(ESBUILD) $(WHERE)/*.js --minify --outdir=$(WHERE)/minified
	mv -v $(WHERE)/minified/*.js $(WHERE)
	rm -r $(WHERE)/minified

.PHONY: all deploy
