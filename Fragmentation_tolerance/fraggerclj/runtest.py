
#This script runs the benchmark over different increment values and saves the graph as pdf

import os
import commands
import subprocess
import numpy as np
import csv
import re

def run(st,inc):
    start = st *1024 *1024
    increment = inc *1024 
    values = [x for x in range(start,(start + (increment * 3)),increment)]
    res = []
    for i in values:
        com = "taskset 0x1 lein with-profile +minheap run"
        heapcomm = "export JVM_OPTS=\-Xms"+str(i)+"\ -Xmx"+str(i)
        #os.system(heapcomm)
        result =subprocess.check_output(heapcomm+" && "+com, shell=True)
        print(result)
        res.append(result)
    print("Plotting graph\n")
    a = [x.split('\n') for x in res]
    free = [float(x) for x,y,z in a]
    res = [re.findall("\d+\.\d+",x) for y,x,z in a]
    res = [float(x) for [x] in res]
    values = [float(x) for x in values]
    d = zip(values,res,free)
    name = "clj_"+str(inc)+"k.txt"
    with open(name,'wb') as myFile:
        wr = csv.writer(myFile, delimiter=',')
        for a,b,c in d:
            e =[a,b,c]
            wr.writerow(e)

   

def main():
    for i in range(100,300,50):
        run(30,i)



main()

