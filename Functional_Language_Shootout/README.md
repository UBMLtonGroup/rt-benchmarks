This is the language shootout to measure relative performance adapted from 
The Computer Language Benchmarks Game. (http://benchmarksgame.alioth.debian.org/)
The programs with highest performance for respective langagues have been chosen for this bencmark

HASKELL
===================================================

notes, command-line, and program output

NOTES:
64-bit Ubuntu quad core
The Glorious Glasgow Haskell Compilation System, version 8.0.1


Sun, 22 May 2016 16:28:41 GMT

MAKE:
mv mandelbrot.ghc-2.ghc mandelbrot.ghc-2.hs
/usr/local/src/ghc-8.0.1/bin/ghc --make -fllvm -O2 -XBangPatterns -threaded -rtsopts  mandelbrot.ghc-2.hs -o mandelbrot.ghc-2.ghc_run
[1 of 1] Compiling Main             ( mandelbrot.ghc-2.hs, mandelbrot.ghc-2.o )
Linking mandelbrot.ghc-2.ghc_run ...
rm mandelbrot.ghc-2.hs
1.85s to complete and log all make actions

COMMAND LINE:
./mandelbrot.ghc-2.ghc_run +RTS -N4 -RTS 16000

(BINARY) PROGRAM OUTPUT NOT SHOWN
    



CLOJURE
====================================================

notes, command-line, and program output

NOTES:
64-bit Ubuntu quad core
Clojure 1.8.0
java version "1.8.0_45"
Java(TM) SE Runtime Environment (build 1.8.0_45-b14)
Java HotSpot(TM) 64-Bit Server VM (build 25.45-b02, mixed mode)


Sat, 27 Feb 2016 17:53:00 GMT

MAKE:
mv mandelbrot.clojure-7.clojure mandelbrot.clj
/usr/local/src/jdk1.8.0_45/bin/java -Dclojure.compiler.direct-linking=true -Dclojure.compile.path=. -cp .:/usr/local/src/clojure/clojure-1.8.0.jar clojure.lang.Compile mandelbrot
Compiling mandelbrot to .
1.24s to complete and log all make actions

COMMAND LINE:
/usr/local/src/jdk1.8.0_45/bin/java -server -XX:+TieredCompilation -XX:+AggressiveOpts -XX:+UseBiasedLocking -Xmx128m -cp .:/usr/local/src/clojure/clojure-1.8.0.jar mandelbrot 16000

(BINARY) PROGRAM OUTPUT NOT SHOWN
    




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
    

