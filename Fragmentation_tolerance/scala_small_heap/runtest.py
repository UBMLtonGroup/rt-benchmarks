
#This script runs the benchmark over different increment values and saves the graph as pdf

import os
import commands
import subprocess
import numpy as np
import csv

def run(st,inc):
    start = st *1024 *1024
    increment = inc *1024 
    values = [x for x in range(start,(start + (increment * 50)),increment)]
    res = []
    for i in values:
        com = "taskset 0x1 scala -J-Xms"+str(i)+" -J-Xmx"+str(i)+" -J-XX:+UseSerialGC fragger2.scala"
        #com = "scalac fragger.scala && taskset 0x1 java -cp .:/usr/share/java/scala-library.jar -Xms"+str(i)+" -Xmx"+str(i)+" -XX:-UseGCOverheadLimit fragger"
        result =subprocess.check_output(com, shell=True)
        print(result)
        res.append(result)
    print("Plotting graph\n")
    print(res)
    a = [x.split(',') for x in res]
    res = [int(x)/1000000 for x,y in a]
    free = [int(y)/1000000 for x,y in a]
    values = [x/1000000 for x in values]
    d = zip(values,res,free)
    name = "scala_"+str(inc)+"k.txt"
    with open(name,'wb') as myFile:
        wr  = csv.writer(myFile, delimiter=',')
        for a,b,c in d:
            e =[a,b ,c]
            wr.writerow(e)

   

def main():
    for i in range(100,1000,50):
        run(35,i)



main()

