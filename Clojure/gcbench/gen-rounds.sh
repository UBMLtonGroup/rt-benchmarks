for i in 1300 1400 1500 1600 1700 1800 1900 2000 2100 2200 2300 2400 2500 ; do echo $i ; taskset -c 0 java -jar target/gcbench-1.0.0-SNAPSHOT-standalone.jar -d $i -e 24 -i 500 2> ${i}.err 1> ${i}.txt ; done
