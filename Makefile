web:
	rm -f web/*js
	dune build web/live.bc.js
	cp _build/default/web/*js web/
