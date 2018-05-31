# Benchmark Erlang Portion

Erlang version: V8.0.2
<br />
<br />
### Compiling   
    erlc gcBench.erl

### Running
    erl -noshell -run gcBench main -t -d -i -s -g -e -s
    erl -noshell -run gcBench main 1 1000000 100 1 1 17 -s
