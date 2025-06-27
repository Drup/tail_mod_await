.PHONY: default
default: build

.PHONY: build
build: 
	dune build @install

.PHONY: testy
test:
	dune runtest

.PHONY: clean
clean:
	dune clean

.PHONY: doc
doc:
	dune build @doc

.PHONY: bench
bench:
	dune exec ./bench/bench_tree.exe
	jq -f bench/extract_from_bechamel.jq short.raw.json > short.json
	jq -f bench/extract_from_bechamel.jq long.raw.json > long.json
	python bench/multiseries.py short.json 
	python bench/multiseries.py long.json 
