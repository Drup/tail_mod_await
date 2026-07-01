#!/bin/sh

for f in *.raw.json; do
    echo "Loading file $f"
    base=$(basename -s ".raw.json" "$f")
    jq -f bench/extract_from_bechamel.jq $base.raw.json > $base.json;
    python3 bench/$base.py $base.json&
done
