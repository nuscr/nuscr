all:
	dune build live.bc.js

deploy:
	mkdir -p $(WHERE)
	cp -v ../_build/default/web/*js index.html style.css $(WHERE)
