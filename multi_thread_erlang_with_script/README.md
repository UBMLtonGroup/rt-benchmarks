1. Presenting runtime of Fibonacci (37th number) calculation, and GCBench.
2. I made graphs of two things - one with separate threads, the other with multi-threads.
3. I attached graphs of 50-iterations and 400-iterations created by Python.

<br />
Erlang - data processing, Python - making graphs.
<br />
<br />
Compile: <br />
Step 1 - <br />
in command line,  erl test.erl    <br />
1> c(test).  <br />
2> test:start_fib(10).   % if you want 10 iterations  <br />
3> test:start_gc(10).   <br />
4> test:start_both(10).  <br />

Step 2 - <br />
py data_parsing.py  # this will genearate graphs created by python based on csv files created in Step 1. <br />


Note: <br />
1. I made Step 1 by executing different functions separately because it resulted in more runtime when executing all of them in one function. <br />
2. I wasn't really able to compile and run erlang files via python file. I will search more to see if I can implement it. <br />
