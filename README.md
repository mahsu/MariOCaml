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

##Notes
* Alternatively, to build the files, run `./build.sh`.
* Note that the game might be slower on a VM depending on your VM configuration. The game should run at 60fps in a normal desktop environment. If it does not, it might be a good idea to copy the files out of the VM after compiling.
