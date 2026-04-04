# clean, compile and serve at http://localhost:8000
all:
	make clean; make build; make web

clean:
	dune clean

# the resulting working website is to be found in _build/default/
build:
	dune build

# serve the page locally, see http://localhost:8000
web:
	python3 -m  http.server 8000
