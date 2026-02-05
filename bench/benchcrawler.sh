#!/bin/bash
set -eu
NBCPUS=$1
REPEATS=$2
HOST=$3

BIN=./_build/default/examples/webcrawler.exe
dune build $BIN

OUT=webcrawler.csv

echo "Writing in $OUT"

printf "Mode,Run,CPUs,Time(h),Time(ns),Size(Mb)\n" > $OUT
for mode in "breath" "depth"
do
    cpus=0
    echo "Running $mode"
    for round in $(seq 1 $REPEATS)
    do
        printf "+"
        printf "${mode},${round},${cpus}," >> $OUT
        $BIN $mode ${cpus} "http://$HOST:8080/content/wikipedia_en_top_mini_2025-12/Lyon" >> webcrawler.csv
    done
    printf "/\n"
done

for mode in "tree" "graph"
do
    for cpus in $(seq 1 $NBCPUS)
    do
        echo "Running with $mode MODE with $cpus CPUS"
        for round in $(seq 1 $REPEATS)
        do
            printf "+"
            printf "${mode},${round},${cpus}," >> $OUT
            $BIN $mode ${cpus} "http://$HOST:8080/content/wikipedia_en_top_mini_2025-12/Lyon" >> webcrawler.csv
        done
        printf "/\n"
    done
done
