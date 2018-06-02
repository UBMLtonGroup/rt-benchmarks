#!/bin/sh 

erlc complex.erl
echo "Done Compiling"
(taskset -c 0 erl -noshell -run complex main 1 1000000 100 80 0 40 -s | tee comp.txt) 
echo "Done Compute only"
(taskset -c 0 erl -noshell -run complex main 1 1000000 100 80 1 40 -s | tee complex.txt)
echo "Done Complex compute" 

