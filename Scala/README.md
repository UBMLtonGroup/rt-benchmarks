#Scala Benchmark
##Contents
* **GCBench.scala**
            
    Modified version of the Garbage collection benchmark written in Java by John Ellis and Pete Kovac. "The first subphase of the program allocates trees top-down using side effects, while the second subphase allocates trees bottom-up without using side effects." (http://www.larcenists.org/Twobit/benchmarks2.temp.html)

* **Perm9.scala**

* **GCBench_multithread.scala**
    
    This benchmark runs multiple GC Threads and Compute Threads simultaneously. The GC Threads perform the tasks in *GCBench.scala* and the compute threads calculate the sum of fibonacci sequence of a specified number.
    
##Prerequisites
Scala version 2.11.7

##Prerequisites

