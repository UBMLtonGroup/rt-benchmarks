import os
import commands
import subprocess
#import matplotlib.pyplot as plt
import csv

def main():
    start = 35 *1024 *1024
    increment = 600 *1024 
    values = [x for x in range(start,(start + (increment * 50)),increment)]
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
    print(res)
    a = [x.split(',') for x in res]
    res = [int(x)/1000000 for x,y in a]
    free = [int(y)/1000000 for x,y in a]
    values = [x/1000000 for x in values]
    d = zip(values,res,free)
    with open("scalafrag.txt",'wb') as myFile:
        wr = csv.writer(myFile, delimiter=',')
        for a,b,c in d:
            e =[a,b,c]
            wr.writerow(e)


    #plt.plot(values,res)
    #plt.xlabel('Heap size (MB)')
    #plt.ylabel('Time (ms)')
    #plt.title('Scala Fragmentation tolerance \n'+'Start size: '+str(start/(1024*1024))+' MB\n'+'Increment: '+str(increment/(1024))+' kB')

    #plt.show()

main()

