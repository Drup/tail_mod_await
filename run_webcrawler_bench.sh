#!/bin/bash
NBCPUS=${1:-4}
REPEATS=${2:-5}
HOST=${3:-localhost}

if [ "$HOST" == "localhost" ]; then
    echo "## Launching kiwix"
    kiwix-serve -p 8080 -d ../wikipedia_en_top_mini_2025-12.zim
fi

echo "## Launching benchmark"
bash bench/benchcrawler.sh $NBCPUS $REPEATS $HOST

echo "## Output image"
python3 bench/benchcrawler.py webcrawler.csv
