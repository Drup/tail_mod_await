#!/bin/bash
set -euo pipefail
NBCPUS=$1
REPEATS=$2
HOST=$3

BIN=./_build/default/examples/webcrawler.exe
dune build $BIN

for cpus in $(seq 1 $NBCPUS)
do
    echo "Running with $cpus CPUS"
    printf "Run\tTime\tSize\n" > webcrawler_${cpus}_domains.csv
    for round in $(seq 1 $REPEATS)
    do
        printf "+"
        printf "${round}\t" >> webcrawler_${cpus}_domains.csv
        $BIN ${cpus} "http://$HOST:8080/content/wikipedia_en_top_mini_2025-12/Lyon" >> webcrawler_${cpus}_domains.csv
    done
    printf "/\n"
done
