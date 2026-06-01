#!/bin/bash
NBCPUS=${1:-4}
REPEATS=${2:-2}
HOST=${3:-localhost}

if [ "$HOST" == "localhost" ]; then
   kiwix-serve -p 8080 -d ../wikipedia_en_top_mini_2025-12.zim
fi

bash bench/benchcrawler.sh $NBCPUS $REPEATS $HOST
