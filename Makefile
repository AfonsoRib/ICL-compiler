.PHONY: build clean run test

build:
	dune build @all

clean:
	dune clean

run:
	dune exec -- icl

test:
	dune runtest
