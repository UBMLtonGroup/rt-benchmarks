for i in 37 38 39 40 ; do
 taskset -c 0 java -jar target/scala-2.12/GCBench-assembly-0.1.0-SNAPSHOT.jar 1 $i 250 1000 1 15 2&>1 > ${i}.txt
done
