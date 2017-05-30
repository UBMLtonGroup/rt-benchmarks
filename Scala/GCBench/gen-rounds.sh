#!/bin/sh -x

#GCS="SerialGC ParallelGC ConcMarkSweepGC"
GCS="ParallelGC ConcMarkSweepGC"

MAXHEAP=1g  # 984599744

for i in 37 38 39 40 41 ; do 
    for gc in $(echo $GCS) ; do
        echo $i - $gc
        fn="${i}-${gc}"
        echo "#C " taskset -c 0 java -jar target/scala-2.12/GCBench-assembly-0.1.0-SNAPSHOT.jar 1 $i 250 1000 1 15 > ${fn}.txt
        echo "#T Scala GCBench 1gc/1comp/1core/$MAXHEAP $gc ${i}" >> ${rn}.txt
        taskset -c 0 java -XX:+Use${gc} -Xmx$MAXHEAP -jar target/scala-2.12/GCBench-assembly-0.1.0-SNAPSHOT.jar 1 $i 250 1000 1 15 2> ${fn}.err 1>> ${fn}.txt
    done
done

