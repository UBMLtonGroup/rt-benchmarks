
## Working with Clojure

http://stackoverflow.com/questions/10901481/typical-way-of-running-clojure-programs

## Working with binary trees

http://macromancy.com/2014/04/09/data-structures-clojure-trees.html

## Prep'ing the benchmark

Easy way to run Clojure is to install [Leiningen](http://leiningen.org/)

> ubuntu$ sudo apt-get install leiningen

## Running the benchmark

```
cd gcbench
lein run
```

compiling to a jar:

```
cd gcbench
lein uberjar
java -jar target/gcbench-1.0.0-SNAPSHOT-standalone.jar -h
```

