

#This script runs the benchmark over different increment values and saves the graph as pdf

import os
import commands
import subprocess
import numpy as np
import csv
import re

def run(st,inc):
    start = st  
    increment = inc  
    values = [x for x in range(start,(start + (increment * 70)),increment)]
    res = []
    for i in values:
        #com = "mlton -drop-pass flatten -drop-pass local-flatten -drop-pass deep-flatten -drop-pass ref-flatten fragger2.sml && taskset 0x1 ./fragger2 @MLton fixed-heap "+str(i)+"k -- 433259"
        com = "mlton fragger2.sml && taskset 0x1 ./fragger2 @MLton max-heap "+str(i)+"k --"
        result =subprocess.check_output(com, shell=True)
        print(result)
        res.append(result)
    print("Plotting graph\n")
    print(res)
    a = [x.split('\n') for x in res]
    res = [float(x) for y,x,z in a]
    free = [x for x,y,z in a]
    values = [x*1000 for x in values]
    free = [int(x)-int(y) for (x,y) in zip(values,free)]
    d = zip(values,res,free)
    name = "sml_"+str(inc)+"k.txt"
    with open(name,'wb') as myFile:
        wr = csv.writer(myFile, delimiter=',')
        for a,b,c in d:
            e =[a,b,c]
            wr.writerow(e)

   

def main():
    for i in range(50,300,15):
        run(35450,i)



main()

