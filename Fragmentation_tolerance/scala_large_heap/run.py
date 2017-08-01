import os
import commands
import subprocess
import matplotlib.pyplot as plt

def main():
    values = [x for x in range(861*1024*1024,(861*1024*1024 + (20000000 *30)),20000000)]
    #values = [x for x in range(1033895940,(1033895940 + (500000 *100)),500000)]
    #start =28*1024*1024
    #iterations=10
    #values =[start]
    #for j in range(1,iterations):
    #    values.append(int((values[j-1]*0.05) + values[j-1]))
    #print(values)
    res = []
    for i in values:
        #com = "taskset 0x1 scala -J-Xms"+str(i)+" -J-Xmx"+str(i)+" -J-XX:-UseGCOverheadLimit fragger.scala"
        com = "taskset 0x1 scala -J-Xms"+str(i)+" -J-Xmx"+str(i)+" -J-XX:+UseSerialGC fragger2.scala"
        #com = "scalac fragger.scala && taskset 0x1 java -cp .:/usr/share/java/scala-library.jar -Xms"+str(i)+" -Xmx"+str(i)+" -XX:-UseGCOverheadLimit fragger"
        #os.system(com)
        result =subprocess.check_output(com, shell=True)
        print(result)
        res.append(result)
        #os.system("rm *.class")
    print("Plotting graph\n")
    res=[int(x)/1000 for x in res]
    values = [x/1000000 for x in values]
    plt.plot(values,res)
    plt.xlabel('Heap size (MB)')
    plt.ylabel('Time (ms)')
    plt.title('Scala Fragmentation tolerance \n')
    plt.show()

main()

