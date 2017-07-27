import os
import commands
import subprocess
import matplotlib.pyplot as plt
import re
import csv

def main():
    start = 30 * 1024 * 1024 #524288000
    increment = 2 *1024 *1024
    values = [x for x in range(start,(start + (increment * 10)),increment)]
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
    res = [re.findall("\d+\.\d+",x) for x in res]
    res = [float(x) for [x] in res]
    print(res)
    values = [x/1000000 for x in values]
    c = zip(values,res)
    with open("cljfrag.txt",'wb') as myFile:
        wr = csv.writer(myFile, delimiter=',')
        for a,b in c:
            d =[a,b]
            wr.writerow(d)
    #plt.plot(values,res)
    #plt.xlabel('Heap size (MB)')
    #plt.ylabel('Time (ms)')
    #plt.title('Clojure Fragmentation tolerance \n'+'Start size: '+str(start/(1024*1024))+' MB\n'+'Increment: '+str(increment/(1024*1024))+' MB')
    #plt.show()


main()

