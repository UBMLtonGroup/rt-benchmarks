import os
import commands
import subprocess
import matplotlib.pyplot as plt

def main():
    start = 987*1024*1024#1033895937#32*1024*1024
    step = 60000000 #12000000 #2000
    iterations = 20
    values = [x for x in range(start,(start + (step *iterations)),step)]
    res = []
    for i in values:
        com = "javac fragger.java && taskset 0x1 java -Xms"+str(i)+" -Xmx"+str(i)+" -XX:+UseSerialGC fragger"
        result =subprocess.check_output(com, shell=True)
        print(result)
        res.append(result)
        os.system("rm *.class")
    print("Plotting graph\n")
    res=[int(x)/1000 for x in res]
    values = [x/1000000 for x in values]
    plt.plot(values,res)
    plt.xlabel('Heap size (MB)')
    plt.ylabel('Time (us)')
    plt.title('Scala Fragmentation tolerance - start 986 MB heap - 1 large array of objects \n')
    plt.show()

main()

