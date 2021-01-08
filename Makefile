DUNE=dune


all: build

build:
	$(DUNE) build --profile release

install:
	$(DUNE) install

run: build
	./main.native test.c

tests: build
	./main.native --tests

doc:
	cd src/; \
	ocamlbuild -use-ocamlfind -pkgs FrontC octagonai.docdir/index.html; \
	cd ..; \
	mv src/_build/octagonai.docdir/* docs/

clean:
	$(DUNE) clean
	rm -rf src/_build/

.PHONY: build clean
