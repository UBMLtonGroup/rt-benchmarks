#!/bin/sh -x

C="vex t2 python ./load.py"

$C -d stats.sql -f raw/20170830-mlton-gcb.txt -t 20170830-mlton-gcb
$C -d stats.sql -f raw/20170831-mlton-p9.txt -t 20170831-mlton-p9

$C -d stats.sql -f raw/20170919-scala-gcb-loop.txt -t 20170919-scala-gcb-loop
$C -d stats.sql -f raw/20170919-scala-p9-loop.txt -t 20170919-scala-p9-loop

$C -d stats.sql -f raw/20170919-cloj-p9-loop.txt -t 20170919-cloj-p9-loop
$C -d stats.sql -f raw/20170925-cloj-gcb-loop.txt -t 20170925-cloj-gcb-loop
$C -d stats.sql -f raw/20170926-cloj-gcb-fib.txt -t 20170926-cloj-gcb-fib

$C -d stats.sql -f raw/20170927-hask-gcb.txt -t 20170927-hask-gcb
$C -d stats.sql -f raw/20170927-hask-p9.txt -t 20170927-hask-p9

