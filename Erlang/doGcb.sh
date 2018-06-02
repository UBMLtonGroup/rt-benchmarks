#!/bin/sh 

erlc gcBench.erl
echo "Done Compiling"
(taskset -c 0 erl -noshell -run gcBench main 1 1000000 100 1 1 17 -s | tee gcb.txt)
echo "Done GCB" 

