.PHONY: build clean run test

build:
	@dune build @all

clean:
	@dune clean

run:
	@echo "\nCompiling..."
	@dune exec -- icl2
	@java -jar jasmin.jar jasmin.j
	@echo "\nOutput:"
	@java Demo

test:
	@dune runtest
