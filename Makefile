# the resulting working website is to be found in _build/html
all:
	ocamlbuild -use-ocamlfind \
	  -plugin-tag "package(js_of_ocaml.ocamlbuild)" \
	  -no-links \
	  main.d.js

clean:
	ocamlbuild -clean
