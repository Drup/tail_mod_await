#!/bin/bash

echo "System Information"

hostname

lscpu

echo "Building"

dune build ./bench/bench_synchro.exe
dune build ./bench/bench_elemscaling.exe
dune build ./bench/bench_parscaling.exe
dune build ./bench/bench_scheduler.exe

echo "Running"

dune exec ./bench/bench_synchro.exe
dune exec ./bench/bench_elemscaling.exe
dune exec ./bench/bench_parscaling.exe
dune exec ./bench/bench_scheduler.exe

ls *.json
