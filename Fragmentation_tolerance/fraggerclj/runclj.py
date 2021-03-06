import os
import commands
import subprocess
#import matplotlib.pyplot as plt
import re
import csv

def main():
    start = 29 * 1024 * 1024 #524288000
    increment = 500 *1024
    values = [x for x in range(start,(start + (increment * 30)),increment)]
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
    free = [float(x) for q,x,y,z in a]
    #res = [re.findall("\d+\.\d+",x) for y,x,z in a]
    used=[float(x) for x,q,y,z in a]
    res = [float(x) for q,y,x,z in a]
    values = [float(x) for x in values]
    d = zip(values,res,free,used)
    with open("cljfrag.txt",'wb') as myFile:
        wr = csv.writer(myFile, delimiter=',')
        for a,b,c,x in d:
            e =[a,b,c,x]
            wr.writerow(e)
    #plt.plot(values,res)
    #plt.xlabel('Heap size (MB)')
    #plt.ylabel('Time (ms)')
    #plt.title('Clojure Fragmentation tolerance \n'+'Start size: '+str(start/(1024*1024))+' MB\n'+'Increment: '+str(increment/(1024*1024))+' MB')
    #plt.show()


main()

