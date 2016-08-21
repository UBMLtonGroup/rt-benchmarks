import re


def getData (csv_file):
	file_Input = open( csv_file ,'r')
	lines = []
	for line in file_Input:
		if line[0].isdigit() == True:
			lines.append(line)
	return lines


def getGCandFibData():
	with open( "GCBench_Fib.csv" ,'r') as f:
		first_line = f.readline()

		# had to parse the first line of the file
		getWords = '1(.*?):'
		time_reg = "\d{2}[:\.\s]\d{2}[:\.\s]\d{2}[.\.\s]\d{6}"
		words = re.findall( getWords , first_line)
		times = re.findall( time_reg , first_line)
		newLine1 = "1" + words[0] + ": " + times[0]
		newLine2 = "1" + words[1] + ": " + times[1]

		lines = []
		lines.append(newLine1), lines.append(newLine2)

		dummy_line = f.readline()
		readlines = f.readlines()

		for line in readlines:
			lines.append(line)
		return lines


def getFibData (fib_lines):
	Fib_start_time = []
	Fib_run_time = []
	for line in fib_lines:
		if "Fibonacci" in line and "start_time" in line:
 			Fib_start_time.append(line)
		elif "Fibonacci" in line and "end_time" in line:
			Fib_run_time.append(line)
	return (Fib_start_time , Fib_run_time)


def getGCBenchData (gc_lines):
	GC_start_time = []
	GC_run_time = []
	for line in gc_lines:
		if "GCBench" in line and "start_time" in line:
 			GC_start_time.append(line)
		elif "GCBench" in line and "end_time" in line:
			GC_run_time.append(line)
	return (GC_start_time , GC_run_time)


def get_time (time_data):
	time_reg = "\d{2}[:\.\s]\d{2}[:\.\s]\d{2}[.\.\s]\d{6}"
	times = []
	for line in time_data:
		time = re.findall(time_reg, line)
		times.extend(time)

	return times


def getRuntime (runtime_data):
	runtime_reg = "\d+ms"
	runtimes = []
	for line in runtime_data:
		time_took = re.findall(runtime_reg, line)
		time = time_took[0][:-2]
		runtimes.append(int(time))

	return runtimes

def runtime_delta (runtimes):
	time_size = len(runtimes)
	deltas = []
	for i in range(time_size - 1):
		delta = abs( runtimes[i+1] - runtimes[i] )
		deltas.append(delta)

	return deltas



class Stars ():
	stars = "****************************************************************************"
	fourth_stars = stars[0:len(stars)/4]
	#return (stars, fourth_stars)

def strlistToString (list1):
	return ' , '.join(list1)

def intlistToString (list1):
	return str(list1).strip('[]')


def writeResults(start_time, run_time):
	linesToAdd = [ ]

	linesToAdd.append( "Start times : [" + strlistToString(get_time(start_time)) +"]")
	linesToAdd.append( "End times : [" + strlistToString(get_time(run_time)) + "]\n")

	runtime = getRuntime(run_time)
	linesToAdd.append( "runtime : [" + intlistToString(runtime) + "]" )
 	linesToAdd.append( "runtime_delta :   [" + intlistToString( runtime_delta(runtime) ) + "] \n")

	return linesToAdd


def writeAll ():
	linesToAdd = []

	linesToAdd.append( Stars().stars)
	linesToAdd.append( Stars().fourth_stars + "Threading Fibonacci Calculation Only" + Stars().fourth_stars )
	linesToAdd.append( "\nBelow is the data from Fibonacci calculation with 10 iterations of threading : \n")

	Fib_lines = getData("fibonacci.csv")
	(fib_start_time,fib_run_time) = getFibData(Fib_lines)
	linesToAdd.extend( writeResults(fib_start_time, fib_run_time) )

	linesToAdd.append( Stars().stars)
	linesToAdd.append( Stars().fourth_stars + "Threading GCBench Only" + Stars().fourth_stars )
	linesToAdd.append( "\nBelow is the data from GCBench threading with 10 iterations of threading : \n")

	GC_lines = getData("GCBench.csv")
	(gc_start_time, gc_run_time) = getGCBenchData(GC_lines)
	linesToAdd.extend( writeResults(gc_start_time, gc_run_time) )


	linesToAdd.append( Stars().stars)
	linesToAdd.append( Stars().fourth_stars + "Threading both GCBench and Fibonacci" + Stars().fourth_stars )

	GC_Fib_lines = getGCandFibData()
	linesToAdd.append("\nFibonacci Calculation results: \n")
	(fib_start_time2,fib_run_time2) = getFibData(GC_Fib_lines)
	linesToAdd.extend( writeResults(fib_start_time2, fib_run_time2) )

	linesToAdd.append("\nGCBench results: \n")
	(gc_start_time2, gc_run_time2) = getGCBenchData(GC_Fib_lines)
	linesToAdd.extend( writeResults(gc_start_time2, gc_run_time2) )

	return linesToAdd

def writeTotxt ():

	f = open("data.txt",'w')
	lines = writeAll()
	for line in lines:
		f.write(line + "\n")
	f.close()



writeTotxt()








#
