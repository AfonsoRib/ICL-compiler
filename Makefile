.PHONY: build clean run test

build:
	@echo "\n Building... "
	@dune build @all

clean:
	@echo "\n Cleaning... "
	@dune clean

runI:
	@echo "\n Running interpreter... "
	@dune exec -- icl

runC:
	@echo "\n Compiling... "
	@if ! dune exec -- icl2; then \
		echo "Compilation failed, stopping."; \
		exit 1; \
	fi
	@if [ ! -s jasmin.j ]; then \
		echo "Jasmin file is empty, likely due to previous errors. Stopping."; \
		exit 1; \
	else \
		if ! java -jar jasmin.jar jasmin.j; then \
			echo "Jasmin error, stopping."; \
			exit 1; \
		fi; \
	fi
	@echo "\n Output "
	@java Demo

test:
	@echo "\n== Running tests... =="
	@dune runtest

runJ:
	@if [ ! -s jasmin.j ]; then \
		echo "Jasmin file is empty, likely due to previous errors. Stopping."; \
		exit 1; \
	else \
		if ! java -jar jasmin.jar *.j; then \
			echo "Jasmin error, stopping."; \
			exit 1; \
		fi; \
	fi
	@echo "\n Output "
	@java Demo
