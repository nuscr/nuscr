nuScr live
=========


Building and installing
-----------------------
This is the code of the web interface for nuScribble. To build it, use
`make`. Once built, you can deploy it to a directory using `make
deploy WHERE=directory`. This copies all the files needed for the web
interface that can then later be uploaded.

Details of the files
--------------------

- `webutils`: some useful primitives to work with `js_of_ocaml`
- `interface`: code to manipulate the web interface
- `live`: glue that links the `interface` with `nuscrlib`
- `genex.sh`: generates `examples.ml` from the directory `/examples`
