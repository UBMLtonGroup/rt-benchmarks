

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
	os.system('erlc gcbench.erl')
	os.system(erl_command)

	outputs = commands.getoutput(erl_command).split('\n')

	writeCSV(outputs, erl)
	#os.chdir('..')

def run_hask(hask, hask_command):
	os.chdir(hask) # 'Haskell'
	os.system('ghc gcbench.hs')
	os.system(hask_command)

	outputs = commands.getoutput(hask_command).split('\n') # can be modified
	writeCSV(outputs, hask)
    #os.chdir('..')

def run_cloj(cloj, cloj_command):
	os.chdir(cloj) #'Clojure'
	os.chdir('gcbench')
	os.system(cloj_command)
	outputs = commands.getoutput(cloj_command).split('\n')
	os.chdir('..')
	writeCSV(outputs, cloj)

def run_scala(scala, scala_command):
	os.chdir(scala) # 'Scala'
	os.system('scalac GCBench_multithread.scala')
	os.system(scala_command)
	outputs = commands.getoutput(scala_command).split('\n')

	writeCSV(outputs, scala)
	#os.chdir('..') # done in writeCSV function

def run_rkt(rkt, rkt_command):
	os.chdir(rkt) # 'Racket'
	os.system(rkt_command)
	outputs = commands.getoutput(rkt_command).split('\n')
	writeCSV(outputs, rkt)


def add_decimal(lang, time):
	if lang == 'Clojure' or lang == 'Haskell' or lang == 'Scala':
		time = time[:10] + '.' + time[10:]
	return float(time)



# parse outputs
def parseOutputs(outputs):

	comp_start = 'comp:start:'
	comp_start_len = len(comp_start)

	comp_stop = 'comp:stop:'
	comp_stop_len = len(comp_stop)

	comp_start2 = 'compute:start:'
	comp_start_len2 = len(comp_start2)

	comp_stop2 = 'compute:stop:'
	comp_stop_len2 = len(comp_stop2)

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
		elif ( line.startswith(comp_start2) ):
			computes.append( line[comp_start_len2:] ) # getting rid of \n
		elif ( line.startswith(comp_stop2) ):
			computes.append( line[comp_stop_len2:])
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
	temp_thread_id = -1
	for i in range(0, size , 2):
		split_outputs = outputs[i].split(':')
		thread_id = split_outputs[0]

		#for initial time stamp when thread_id changes
		if (thread_id > temp_thread_id):
			start_delta = 0
			start = split_outputs[2] #  current i
			stop = outputs[i+1].split(':')[2]
			start = add_decimal(lang, start)
			stop = add_decimal(lang, stop)

			run_length = stop - start
			temp_thread_id = thread_id
		else:
			new_start = split_outputs[2]
			stop = outputs[i+1].split(':')[2]
			new_start = add_decimal(lang, new_start)
			stop = add_decimal(lang, stop)

			start_delta = new_start - start
			start = new_start
			run_length = stop - start

		write_all.append([ thread_id, start_delta, run_length ])

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


# Python examples of command lines
# 1 thread
#python runthemall.py -t 1 -d 37 -i 10 -s 1 -g 1 -e 10 -m 4 -S -D
# 3 thread, depth = 20
#python runthemall.py -t 3 -d 37 -i 10 -s 1 -g 3 -e 20 -m 4 -S -D


def make_commandLine(lang, start_line, t, d, i, s, g, e):
	if lang == 'Haskell' or lang == 'Clojure' or lang == 'Racket':
		return start_line + t + ' -d ' + d + ' -i ' + i + ' -s ' + s + ' -g ' + g + ' -e ' + e
	else:
		if lang == 'Erlang':
			return start_line + t + ' ' + d + ' ' + i + ' ' + s + ' ' + g + ' ' + e + ' -s'

		#Scala
		return start_line + t + ' ' + d + ' ' + i + ' ' + s + ' ' + g + ' ' + e



def main():

	parser = argparse.ArgumentParser(description='Process arguments')
	parser.add_argument('-t','--t', help='Compute Threads', default = 1)
	parser.add_argument('-d','--d', help='Compute Depth' , default = 37)
	parser.add_argument('-i','--i', help='Compute/GC Iterations' , default = 10)
	parser.add_argument('-s','--s', help='Compute Sleep' , default = 1)
	parser.add_argument('-g','--g', help='GC Threads' , default = 1)
	parser.add_argument('-e','--e', help='Maximum tree depth to allocate' , default = 10)
	parser.add_argument('-m','--m', help='Maximum heap to allocate (in MB)' , default = 4)
	parser.add_argument('-S','--gc_stats', action = 'store_true', help='Enable printing stats' , default = False)
	parser.add_argument('-D','--debug', action = 'store_true', help='Enable debugging output' , default = False)

	args = parser.parse_args()

	os.chdir('..')
	t = str(args.t); d = str(args.d); i = str(args.i); s = str(args.s); g = str(args.g); e = str(args.e); m = str(args.m)

	erl_command = make_commandLine('Erlang', 'erl -noshell -run gcbench main ', t, d, i, s, g, e)
	#run_erlang('Erlang', erl_command)

	hask_command = make_commandLine('Haskell', './gcbench -t ', t, d, i, s, g, e)
	#run_hask('Haskell', hask_command)

	cloj_command = make_commandLine('Clojure', 'lein run -- -t ', t, d, i, s, g, e)
	#run_cloj('Clojure', cloj_command)

	scala_command = make_commandLine('Scala', 'scala GCBench_multithread ', t, d, i, s, g, e)
	#run_scala('Scala', scala_command)

	rkt_command = make_commandLine('Racket', 'racket gcbench.rkt -t ', t, d, i, s, g, e)
	#run_rkt('Racket', rkt_command)


main()

#
