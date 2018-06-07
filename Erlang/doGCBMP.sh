#!/bin/sh 

erlc gcBenchMP.erl
echo "Done Compiling"
(taskset -c 0 erl -noshell -run gcBenchMP main 1000000 100 1 17 8457100 -s | tee gcbmp.txt)
echo "Done GCB" 

