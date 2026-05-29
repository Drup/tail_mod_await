#!/bin/bash
set -e
NBCPUS=$1
REPEATS=$2
HOST=$3

if [-z ${HOST+x}]; then
   kiwix-serve -p 8080 -d ../wikipedia_en_top_mini_2025-12;
fi

bench/benchcrawler.sh ${NBCPUS:-8} ${REPEATS:-4} ${HOST:-localhost}
