# Synthetic benchmarks: garbage collection

## gcbench

This program was written to mimic the phase structure that has been conjectured for a class of application programs for which garbage collection may represent a significant fraction of the execution time. This benchmark warms up by allocating and then dropping a binary tree of height 18. Then it allocates a permanent tree of height 16 and a permanent array of 500000 floating point numbers. Then it allocates about 350 megabytes of tree storage in seven phases, allocating about 50 megabytes per phase. The first phase allocates 67648 trees of height 4, dropping each tree before allocating the next. The second phase allocates and drops 16512 trees of height 6, and so on to the last phase, which allocates 8 trees of height 16. Each phase is divided into two subphases. The first subphase allocates trees top-down using side effects, while the second subphase allocates trees bottom-up without using side effects. This benchmark was written in Java by John Ellis and Pete Kovac, modified by Hans Boehm, and translated into Scheme, Standard ML, C++, and C by William Clinger.
One glaring difference between this benchmark and typical application code is that none of the trees are traversed after they are constructed. [This puts C and C++ at a disadvantage, because languages that rely on explicit deallocation must traverse the trees to deallocate their storage.]

[Generational garbage collectors perform extremely well on the early phases of this benchmark, but are likely to perform worse than non-generational collectors on the last phase or two.]

## See Also

https://github.com/smlsharp/smlsharp/tree/master/benchmark/benchmarks

http://hboehm.info/gc/gc_bench/applet/GCBench.java

https://github.com/larcenists/larceny/tree/master/test/Benchmarking/GC
