

#This script runs the benchmark over different increment values and saves the graph as pdf

import os
import commands
import subprocess
import numpy as np
import csv
import re

def run(st,inc):
    start = (st *1024) +1
    increment = inc  
    values = [x for x in range(start,(start + (increment * 70)),increment)]
    res = []
    for i in values:
        com = "mlton fragger2.sml && taskset 0x1 ./fragger2 @MLton fixed-heap "+str(i)+"k -- 433259"
        #os.system(com)
        result =subprocess.check_output(com, shell=True)
        print(result)
        res.append(result)
    print("Plotting graph\n")
    print(res)
    a = [x.split('\n') for x in res]
    res = [float(x) for x,y in a]
    #free = [int(y)/1000000 for x,y in a]
    values = [x for x in values]
    d = zip(values,res)
    name = "sml_"+str(inc)+"k.txt"
    with open(name,'wb') as myFile:
        wr = csv.writer(myFile, delimiter=',')
        for a,b in d:
            e =[a,b]
            wr.writerow(e)

   

def main():
    for i in range(100,1000,50):
        run(30,i)



main()

