# Racket GCBench Benchmark #

## Contents ##

##### gcbench.rkt #####
The multithreaded version of the GCBench benchmark supporting command line arguments to specify test parameters. A reimplementation of the scheme version using structures in place of records.


##### perm9.rkt #####
The multithreaded version of the Perm9 benchmark supporting command line arguments to specify test parameters.  A port of of the scheme version. Uses mutable state like the original scheme version does.


## Compiling and Running the Benchmark ##

### Dependencies ###
* Racket (tested with v6.6)

### Running the Benchmark ###
    racket gcbench.rkt
    racket perm9.rkt
