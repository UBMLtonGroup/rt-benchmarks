import os
import commands
import subprocess
#import matplotlib.pyplot as plt
import csv

def main():
    start = 30721 
    increment = 200 #kb
    values = [x for x in range(start,(start + (increment * 30)),increment)]
    #values = [x for x in range(1033895940,(1033895940 + (500000 *100)),500000)]
    #start =28*1024*1024
    #iterations=10
    #values =[start]
    #for j in range(1,iterations):
    #    values.append(int((values[j-1]*0.05) + values[j-1]))
    #print(values)
    res = []
    for i in values:
        com = "mlton fragger2.sml && taskset 0x1 ./fragger2 @MLton fixed-heap "+str(i)+"k -- 433259"
        #os.system(com)
        result =subprocess.check_output(com, shell=True)
        print(result)
        res.append(result)
        #os.system("rm *.class")
    print("Plotting graph\n")
    print(res)
    a = [x.split('\n') for x in res]
    res = [float(x) for x,y in a]
    #free = [int(y)/1000000 for x,y in a]
    values = [x/1000 for x in values]
    d = zip(values,res)
    with open("smlfrag.txt",'wb') as myFile:
        wr = csv.writer(myFile, delimiter=',')
        for a,b in d:
            e =[a,b]
            wr.writerow(e)


    #plt.plot(values,res)
    #plt.xlabel('Heap size (MB)')
    #plt.ylabel('Time (ms)')
    #plt.title('Scala Fragmentation tolerance \n'+'Start size: '+str(start/(1024*1024))+' MB\n'+'Increment: '+str(increment/(1024))+' kB')

    #plt.show()

main()

