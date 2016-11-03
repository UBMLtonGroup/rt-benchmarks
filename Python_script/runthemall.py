

import sys, getopt
import argparse
import os, commands
import subprocess
import csv
import time


def validArguments(args):
	args_hash = vars(args)
	arg_values = args_hash.values()
	for value in arg_values:
		value = int(value)
		if value < 0 or value > 65536:
	 		print('Argument must be a number between 0 and 65536')
			exit()



#scala GCBench # # # # # #
def run_scala(scala, scala_command):
	os.chdir(erl) # 'Scala'
	os.system('scalac GCBench.scala')
	os.system('scala GCBench')

	outputs = commands.getoutput(erl_command).split('\n')

	writeCSV(outputs, erl)
	#os.chdir('..')

def run_erlang(erl, erl_command):
	os.chdir(erl) # 'Erlang'
	os.system('erlc gcbench.erl')
	os.system(erl_command)
	outputs = commands.getoutput(erl_command).split('\n')

	writeCSV(outputs, erl)
	#os.chdir('..')

def run_hask(hask, hask_command):
	os.chdir(hask) # 'Haskell'
	os.system('ghc gcbench.hs')
	outputs = commands.getoutput(hask_command).split('\n') # can be modified
	writeCSV(outputs, hask)
    #os.chdir('..')

def run_cloj(cloj, cloj_command):
	os.chdir(cloj) #'Clojure'
	os.system('cd gcbench')
	outputs = commands.getoutput(cloj_command).split('\n')
	os.chdir('..')
	writeCSV(outputs, cloj)

# parse outputs
def parseOutputs(outputs):
	comp_start = 'comp:start:'
	comp_start_len = len(comp_start)

	comp_stop = 'comp:stop:'
	comp_stop_len = len(comp_stop)

	gc_start = 'gc:start:'
	gc_start_len = len(gc_start)

	gc_stop = 'gc:stop:'
	gc_stop_len = len(gc_stop)


	computes = []
	gcs = []

	for line in outputs:
		if ( line.startswith(comp_start) ):
			computes.append( line[comp_start_len:] ) # getting rid of \n
		elif ( line.startswith(comp_stop) ):
			computes.append( line[comp_stop_len:])
		elif ( line.startswith( gc_start ) ):
			gcs.append( line[gc_start_len:] )
		elif ( line.startswith(gc_stop) ):
			gcs.append( line[gc_stop_len:])


	computes = sorted(computes)
	gcs = sorted(gcs)

	return computes, len(computes), gcs, len(gcs)


#processing the parsed outputs
def processOutputs(outputs, size, lang):

	write_all = []
	start = 0
	for i in range(0, size , 2):
		split_outputs = outputs[i].split(':')
		thread_id = split_outputs[0]
		time = float(split_outputs[2])

		if lang == 'Clojure':
			time_temp = str(time)
			time = time_temp[:10] + '.' + time_temp[10:]
			time = float(time)

		run_length = time - start
		start = time

		stop_time = float(outputs[i+1].split(':')[2])
		start_delta = stop_time - time
		write_all.append([ thread_id, start_delta , thread_id ])

	return write_all


#write parsed outputs to csv files
def writeCSV(outputs, lang):
	computes, comp_size, gcs, gcs_size = parseOutputs(outputs)

	comp_write_all = processOutputs( computes, comp_size, lang)
	gcs_write_all = processOutputs( gcs, gcs_size, lang)

	os.chdir('..') # write csv files in the home directory
	with open( 'comp_' + lang + ".csv", "wb") as f:
		writer = csv.writer(f)
		writer.writerows(comp_write_all)

	with open( 'gc_' + lang + ".csv", "wb") as f:
		writer = csv.writer(f)
		writer.writerows(gcs_write_all)



#run_python = 'python runthemall.py -t 1 -d 37 -i 10 -s 1 -g 1 -e 10 -m 4 -D'
def main():
	parser = argparse.ArgumentParser(description='Process arguments')
	parser.add_argument('-t','--t', help='Compute Threads', default = 1)
	parser.add_argument('-d','--d', help='Compute Depth' , default = 37)
	parser.add_argument('-i','--i', help='Compute/GC Iterations' , default = 10)
	parser.add_argument('-s','--s', help='Compute Sleep' , default = 1)
	parser.add_argument('-g','--g', help='GC Threads' , default = 1)
	parser.add_argument('-e','--e', help='Maximum tree depth to allocate' , default = 10)
	parser.add_argument('-m','--m', help='Maximum heap to allocate (in MB)' , default = 4)
	parser.add_argument('-D','--debug', action = 'store_true', help='Enable debugging output' , default = False)
	args = parser.parse_args()

	t = str(args.t); d = str(args.d); i = str(args.i); s = str(args.s); g = str(args.g); e = str(args.e); m = str(args.m)
	erl_command = 'erl -noshell -run gcbench main ' + t + ' ' + d + ' ' + i + ' ' + s + ' ' + g + ' ' + e + ' ' + m + ' -s'# init stop'
	run_erlang('Erlang', erl_command)
	#hask_command = 'gcbench ' + ' ... (arguments) '
	#run_hask('Haskell', hask_command)
	#cloj_command = 'lein run -- -t # -d # -i # -s # -g # -e # -m # -S -D -h'
	#run_cloj('Clojure', cloj_command)


main()




#
