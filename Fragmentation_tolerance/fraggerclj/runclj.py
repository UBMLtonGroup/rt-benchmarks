import os
import commands
import subprocess
import matplotlib.pyplot as plt
import re

def main():
    values = [x for x in range(524288500,(524288500 + (50000000 *100)),50000000)]
    res =[]
    print("Starting script\n")
    for i in values:
        com = "lein with-profile +minheap run"
        heapcomm = "export JVM_OPTS=\-Xms"+str(i)+"\ -Xmx"+str(i)
        #os.system(heapcomm)
        result =subprocess.check_output(heapcomm+" && "+com, shell=True)
        print(result)
        res.append(result)
    print("Plotting graph\n")
    res = [re.findall("\d+\.\d+",x) for x in res]
    print(res)
    plt.plot(values,res)
    plt.xlabel('Heap size (bytes)')
    plt.ylabel('Time (ms)')
    plt.title('Clojure Fragmentation tolerance \n')
    plt.show()


main()

