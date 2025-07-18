#!/bin/bash

for f in *.raw.json; do
    echo "Loading file $f"
    base=$(basename -s ".raw.json" "$f")
    jq -f bench/extract_from_bechamel.jq $base.raw.json > $base.json;
    python bench/$base.py $base.json&
done
