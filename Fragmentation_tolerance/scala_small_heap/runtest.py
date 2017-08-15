
#This script runs the benchmark over different increment values and saves the graph as pdf

import os
import commands
import subprocess
import matplotlib.pyplot as plt
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
    with open("scalafrag.txt",'wb') as myFile:
        wr  = csv.writer(myFile, delimiter=',')
        for a,b,c in d:
            e =[a,b ,c]
            wr.writerow(e)

    heap, time , free = np.loadtxt('scalafrag.txt', delimiter=',', unpack=True)

    fig,ax1 = plt.subplots(dpi=120, figsize=(7,7))
    ax2 = plt.twinx()

    ax1.set_ylabel("Allocation time ($ms$)",color = 'blue')
    ax1.set_xlabel("Initial heap size ($MB$)")

    ax2.set_ylabel("Free space on heap ($MB$)",color = 'green')
    ax2.set_xlabel("Initial heap size ($MB$)")


    p1,= ax1.plot(heap,time, label='Time taken to allocate large array')
    p2,= ax2.plot(heap,free , label='Free space on heap' ,color = 'green')


    plt.title('Scala Fragmentation tolerance')

    from matplotlib.font_manager import FontProperties
    fontP = FontProperties()
    fontP.set_size('small')


    lines =[p1,p2]

    plt.legend(lines, [l.get_label() for l in lines],prop = fontP ,loc =9)

    name = "scala_"+str(inc)+"k.png"

    plt.savefig(name)


def main():
    for i in range(100,1000,50):
        run(35,i)



main()

