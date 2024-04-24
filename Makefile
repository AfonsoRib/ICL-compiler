.PHONY: build clean run

build:
	dune build @all

clean:
	dune clean

run:
	dune exec -- icl
