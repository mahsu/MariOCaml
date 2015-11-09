MariOCaml
=============

##Building the Project
1. First, ensure that js\_of\_ocaml and its dependencies are installed by running:

        opam install js_of_ocaml
2. With the cs3110 tool installed, run:

        cs3110 compile -p js_of_ocaml -p js_of_ocaml.syntax main.ml
to compile the binaries to the `_build` folder.

3. To cross-compile to javascript, run

        js_of_ocaml _build/main.d.byte

4. Open `index.html` to run!
