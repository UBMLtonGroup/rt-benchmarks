import os
import commands
import subprocess
#import matplotlib.pyplot as plt
import csv

def main():
    start = 35450 
    increment = 75 #kb
    values = [x for x in range(start,(start + (increment * 70)),increment)]
    #values = [x for x in range(1033895940,(1033895940 + (500000 *100)),500000)]
    #start =28*1024*1024
    #iterations=10
    #values =[start]
    #for j in range(1,iterations):
    #    values.append(int((values[j-1]*0.05) + values[j-1]))
    #print(values)
    res = []
    for i in values:
        #com = "mlton -drop-pass flatten -drop-pass local-flatten -drop-pass deep-flatten -drop-pass ref-flatten fragger2.sml && taskset 0x1 ./fragger2 @MLton fixed-heap "+str(i)+"k -- 433259"
        com = "mlton fragger2.sml && taskset 0x1 ./fragger2 @MLton max-heap "+str(i)+"k --"
        #os.system(com)
        result =subprocess.check_output(com, shell=True)
        print(result)
        res.append(result)
        #os.system("rm *.class")
    print("Plotting graph\n")
    print(res)
    a = [x.split('\n') for x in res]
    res = [float(x) for y,x,z in a]
    free = [x for x,y,z in a]
    values = [x*1000 for x in values]
    free = [int(x)-int(y) for (x,y) in zip(values,free)]
    d = zip(values,res,free)
    with open("smlfrag.txt",'wb') as myFile:
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

