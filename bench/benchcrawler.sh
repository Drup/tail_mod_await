#!/bin/bash
set -eu
NBCPUS=$1
REPEATS=$2
HOST=$3

BIN=./_build/default/examples/webcrawler.exe
dune build $BIN

OUT=webcrawler.csv

echo "Writing in $OUT"

printf "Run\tCPUs\tTime(h)\tTime(ns)\tSize\n" > $OUT
for cpus in $(seq 1 $NBCPUS)
do
    echo "Running with $cpus CPUS"
    for round in $(seq 1 $REPEATS)
    do
        printf "+"
        printf "${round}\t${cpus}\t" >> $OUT
        $BIN ${cpus} "http://$HOST:8080/content/wikipedia_en_top_mini_2025-12/Lyon" >> webcrawler.csv
    done
    printf "/\n"
done
