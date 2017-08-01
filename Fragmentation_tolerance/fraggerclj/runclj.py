import os
import commands
import subprocess
#import matplotlib.pyplot as plt
import re
import csv

def main():
    start = 30 * 1024 * 1024 #524288000
    increment = 1 *1024 *1024
    values = [x for x in range(start,(start + (increment * 50)),increment)]
    res =[]
    print("Starting script\n")
    for i in values:
        com = "taskset 0x1 lein with-profile +minheap run"
        heapcomm = "export JVM_OPTS=\-Xms"+str(i)+"\ -Xmx"+str(i)
        #os.system(heapcomm)
        result =subprocess.check_output(heapcomm+" && "+com, shell=True)
        print(result)
        res.append(result)
    print("Plotting graph\n")
    a = [x.split('\n') for x in res]
    free = [float(x)/1000000 for x,y,z in a]
    res = [re.findall("\d+\.\d+",x) for y,x,z in a]
    res = [float(x) for [x] in res]
    values = [x/1000000 for x in values]
    d = zip(values,res,free)
    with open("cljfrag.txt",'wb') as myFile:
        wr = csv.writer(myFile, delimiter=',')
        for a,b,c in d:
            e =[a,b,c]
            wr.writerow(e)
    #plt.plot(values,res)
    #plt.xlabel('Heap size (MB)')
    #plt.ylabel('Time (ms)')
    #plt.title('Clojure Fragmentation tolerance \n'+'Start size: '+str(start/(1024*1024))+' MB\n'+'Increment: '+str(increment/(1024*1024))+' MB')
    #plt.show()


main()

