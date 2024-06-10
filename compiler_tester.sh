#!/bin/sh

# OUTPUT="$(make runC < test.txt)"

#!/bin/sh

compile() {
    echo "\n Compiling... "
    if ! dune exec -- icl2; then
        echo "Compilation failed, stopping."
        exit 1
    fi
    cd jasmin_bytecode
    if [ ! -s jasmin.j ]; then
        echo "Jasmin file is empty, likely due to previous errors. Stopping."
        exit 1
    else
        if ! java -jar ../jasmin.jar *.j; then
            echo "Jasmin error, stopping."
            exit 1
        fi
    fi
    cd ..
}

run() {
    cd jasmin_bytecode
    java Demo
    cd ..
    rm -r jasmin_bytecode
}

rm log.txt
while IFS= read -r line; do
    echo "Running test: $line"
    echo "Running test: $line" >> log.txt
    echo "$line" | compile > /dev/null && OUTPUT="$(run)"
    echo "\nOutput:\n$OUTPUT" >> log.txt
    echo "\nOutput:\n$OUTPUT"    
done < testes_comp
