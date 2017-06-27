import os
import commands
import subprocess
import matplotlib.pyplot as plt

def main():
    values = [x for x in range(1033895940,(1033895940 + (5000000 *20)),5000000)]
    res = []
    for i in values:
        com = "taskset 0x1 scala -J-Xms"+str(i)+" -J-Xmx"+str(i)+" -J-XX:-UseGCOverheadLimit fragger.scala"
        #os.system(com)
        result =subprocess.check_output(com, shell=True)
        print(result)
        res.append(result)
    print("Plotting graph\n")
    res=[int(x)/1000 for x in res]
    plt.plot(values,res)
    plt.xlabel('Heap size (bytes)')
    plt.ylabel('Time (ms)')
    plt.title('Scala Fragmentation tolerance \n')
    plt.show()

main()

