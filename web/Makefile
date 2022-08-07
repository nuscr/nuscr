WHERE   := ../_site

all:
	cd .. && dune build @doc
	dune build live.bc.js

deploy: all
	mkdir -p $(WHERE)
	mkdir -p $(WHERE)/docs
	cp -v ../_build/default/web/*.js index.html style.css $(WHERE)
	cp -v ../doc/logo/nuScr.png $(WHERE)
	chmod -R +w $(WHERE)
	cp -r ../_build/default/_doc/_html/* $(WHERE)/docs

minify: deploy
	./minify_all.sh $(WHERE)

.PHONY: all deploy
