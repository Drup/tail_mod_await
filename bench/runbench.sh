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

echo "Running Synchro"
dune exec ./bench/bench_synchro.exe
echo "Done Synchro"

echo "Running Elescaling"
dune exec ./bench/bench_elemscaling.exe
echo "Done Elescaling"

echo "Running Parscaling"
dune exec ./bench/bench_parscaling.exe
echo "Done Parscaling"

echo "Running Scheduler"
dune exec ./bench/bench_scheduler.exe
echo "Done Scheduler"

echo "Available files:"
ls *.json
