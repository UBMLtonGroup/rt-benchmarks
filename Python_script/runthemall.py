
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


def run_erlang(erl, erl_command):
	os.chdir(erl) # 'Erlang'
	os.system('erlc benchmark.erl')
	#os.system(erl_command)
	outputs = commands.getoutput(erl_command).split('\n')

	writeCSV(outputs, erl)
	#os.chdir('..')

def run_hask(hask):
	os.chdir(hask) # 'Haskell'
	os.system('ghc gcbench.hs')
	outputs = commands.getoutput('gcbench').split('\n') # can be modified
	writeCSV(outputs, hask)
    #os.chdir('..')

def run_cloj(cloj):
	os.chdir(cloj) #'Clojure'
	outputs = commands.getoutput('lein run -- -h').split('\n')
	writeCSV(outputs, cloj)
	#os.chdir('..')

# parse outputs
def parseOutputs(outputs):
	start = 'comp:start:'
	start_len = len(start)

	stop = 'comp:stop:'
	stop_len = len(stop)

	computes = []

	for line in outputs:
		if ( line.startswith(start) ):
			computes.append( line[start_len:] ) # getting rid of \n
		elif ( line.startswith(stop) ):
			computes.append( line[stop_len:])


	computes = sorted(computes)

	return computes, len(computes)


#write parsed outputs to csv files
def writeCSV(outputs, lang):
	computes, size = parseOutputs(outputs)
	print computes

	write_all = []
	start = 0
	for i in range(0, size , 2):
		split_computes = computes[i].split(':')
		thread_id = split_computes[0]
		time = float(split_computes[2])

		run_length = time - start
		start = time

		stop_time = float(computes[i+1].split(':')[2])
		start_delta = stop_time - time
		write_all.append([ thread_id, start_delta , thread_id ])

	os.chdir('..') # write csv file in the home directory
	with open( lang + ".csv", "wb") as f:
		writer = csv.writer(f)
		writer.writerows(write_all)



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

	t = args.t; d = args.d; i = args.i; s = args.s; g = args.g; e = args.e; m = args.m
	erl_command = 'erl -noshell -run benchmark main ' + t + ' ' + d + ' ' + i + ' ' + s + ' ' + g + ' ' + e + ' ' + m + ' -s init stop'

	#run_erlang('Erlang', erl_command)
	hask_command = 'gcbench ' + ' ... (arguments) '
	#run_hask('Haskell', hask_command)
	cloj_command = 'run - ' + ' ... (arguments) '
	#run_cloj('Clojure', cloj_command)


main()




#
