#!/bin/sh -x
mlton gcbench.sml
./gcbench @MLton gc-summary fixed-heap 13000k -- 
