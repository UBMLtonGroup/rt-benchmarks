This is the language shootout to measure relative performance adapted from 
The Computer Language Benchmarks Game. (http://benchmarksgame.alioth.debian.org/)
The programs with highest performance for respective langagues have been chosen for this bencmark


RACKET
=====================================================

notes, command-line, and program output

NOTES:
64-bit Ubuntu quad core
Welcome to Racket v6.6.


Sat, 23 Jul 2016 23:18:23 GMT

MAKE:
/usr/local/src/racket-6.6/bin/raco make mandelbrot.racket-4.racket
0.57s to complete and log all make actions

COMMAND LINE:
/usr/local/src/racket-6.6/bin/racket ./compiled/mandelbrot.racket-4_racket.zo 16000

(BINARY) PROGRAM OUTPUT NOT SHOWN


ERLANG (HiPE)
====================================================
NOTES:
64-bit Ubuntu quad core
Erlang/OTP 19 [erts-8.0] [source] [64-bit] [smp:4:4] [async-threads:10] [hipe] [kernel-poll:false]


Thu, 23 Jun 2016 17:32:19 GMT

MAKE:
mv mandelbrot.hipe-2.hipe mandelbrot.erl
/usr/local/src/otp_src_19.0/bin/erlc +native +"{hipe, [o3]}" mandelbrot.erl
0.54s to complete and log all make actions

COMMAND LINE:
/usr/local/src/otp_src_19.0/bin/erl -smp enable -noshell -run  mandelbrot main 16000

(BINARY) PROGRAM OUTPUT NOT SHOWN
    

