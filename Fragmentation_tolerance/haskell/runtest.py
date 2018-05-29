
#This script runs the benchmark over different increment values and saves the graph as pdf

import os
import commands
import subprocess
import numpy as np
import csv
import re

def run(st,inc):
    res = []
    count =1
    for i in values:
        #com = "taskset 0x1 scala -J-Xms"+str(i)+" -J-Xmx"+str(i)+" -J-XX:-UseGCOverheadLimit fragger.scala"
        com = "ghc fragger2.hs -rtsopts -XFlexibleContexts -fforce-recomp && taskset 0x1 ./fragger2 +RTS -T -M"+str(i)+" -RTS"
        #com = "scalac fragger.scala && taskset 0x1 java -cp .:/usr/share/java/scala-library.jar -Xms"+str(i)+" -Xmx"+str(i)+" -XX:-UseGCOverheadLimit fragger"
        #os.system(com)
        result =subprocess.check_output(com, shell=True)
        print(result)
        res.append(result)
        print("Iteration: "+str(count))
        count=count+1
        #os.system("rm *.class")
    print("Plotting graph\n")
    #print(res)
    a = [x.split('\n') for x in res]
    res = [re.findall("\d+\.\d+",x) for p,q,y,x,z in a]
    res=[float(x)*1000 for [x] in res]
    free = [x for p,q,x,y,z in a]
    values = [x for x in values]
    free = [int(x)-int(y) for (x,y) in zip(values,free)]
    d = zip(values,res,free)
    name = "hsk_"+str(inc)+"k.txt"
    with open(name,'wb') as myFile:
        wr = csv.writer(myFile, delimiter=',')
        for a,b,c in d:
            e =[a,b,c]
            wr.writerow(e)

   

def main():
    for i in range(50,300,15):
        run(35450,i)



main()

