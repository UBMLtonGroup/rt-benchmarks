# Haskell GCBench Benchmark #

## Contents ##

##### gcbench.hs #####
The multithreaded version of the GCBench supporting command line arguments to specify test parameters. Builds trees bottom-up (makeTree).

##### perm9.hs #####
The multithreaded version of the Perm9 benchmark supporting command line arguments to specify test parameters. Passes partial permutation lists as arguments instead of using mutable state like the scheme version does.

##### alternative.hs #####
A direct translation of the original GCBench benchmark into Haskell, supporting construction of trees both top-down and bottom-up.

## Compiling and Running the Benchmark ##

### Dependencies ###
* GHC (uses GHC specific getGCStats; tested with 8.0.1)
* optparse-applicative

### Building with GHC ##
    ghc gcbench.hs
    ghc perm9.hs

### Running the Benchmark ###
    The RTS T flag is required to fetch the GC heap stats

    gcbench +RTS -T
    perm9 +RTS -T

    to limit memory

    gcbench +RTS -M1000m
