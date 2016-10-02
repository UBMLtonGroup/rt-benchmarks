# Haskell GCBench Benchmark #

## Contents ##

##### benchmark.hs #####
The multithreaded version of the GCBench supporting command line arguments to specify test parameters. Builds trees bottom-up (makeTree).

##### alternative.hs #####
A direct translation of the original GCBench benchmark into Haskell, supporting construction of trees both top-down and bottom-up.

## Compiling and Running the Benchmark ##

### Dependencies ###
* GHC (uses GHC specific getGCStats; tested with 8.0.1)
* optparse-applicative

### Building with GHC ##
    ghc benchmark.hs

### Running the Benchmark ###
    benchmark
