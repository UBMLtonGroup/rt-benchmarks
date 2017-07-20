#!/bin/sh

(for i in 37 38 39 40 41 ; do grep compute ${i}.txt | head -10 | tail -2 ; done) | python ../../delta.py 

# we expect the deltas to increase
