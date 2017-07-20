#Scala Benchmark
##Contents
* **GCBench.scala**
            
    Modified version of the Garbage collection benchmark written in Java by John Ellis and Pete Kovac. "The first subphase of the program allocates trees top-down using side effects, while the second subphase allocates trees bottom-up without using side effects." (http://www.larcenists.org/Twobit/benchmarks2.temp.html)
    
* **GCBench_multithread.scala**
    
    This benchmark runs multiple GC Threads and Compute Threads simultaneously. The GC Threads perform the tasks in *GCBench.scala* and the compute threads calculate the sum of fibonacci sequence of a specified number.

* **Perm9.scala**
    
##Prerequisites
Scala version 2.11.7
Java version 1.8.0_66

##Compile
* cd GCBench ; sbt assembly
* java -jar target/scala-2.12/GCBench-assembly-0.1.0-SNAPSHOT.jar

* scalac GCBench.scala
* scalac GCBench_multithread.scala
* scalac Perm9.scala

##Execute
* **Default**
  * scala GCBench
  * scala GCBench_multithread
  * scala Perm9
* **Specified Arguments**
  * scala GCBench -t -d -i -s -g -e -Xms_m
  * scala GCBench_multithread -t -d -i -s -g -e -Xms_m
  * scala Perm9 -t -d -i -s -g -e -Xms_m
  
-t = Compute Threads

-d = Compute depth

-i = Compute/GC Iterations

-s = Compute Sleep

-g = GC Threads

-e = Tree Depth for GC Threads

-Xms_m = Initial memory allocation pool (Actual size to be inputted in place of _)

 scala -J-Xmx2g ...
 