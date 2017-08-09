# perm9


Creates a list containing all 362880 permutations of a list of 9 integers, in grey code order, using Zaks's algorithm, 5 iterations. A test of storage allocation and garbage collection. At the end of each iteration, the oldest half of the live storage becomes garbage.
[This benchmark is particularly difficult for generational garbage collectors, since it violates their assumption that young objects have a shorter future life expectancy than older objects. Nevertheless the generational collectors used in Larceny and in Chez Scheme perform reasonably well.] (http://www.larcenists.org/Twobit/benchmarks2.temp.html)

In our version we allow for multiple heap mutation threads and multiple computation threads to assess the impact of garbage collection on computation periodicity.



## Usage

```
$ lein uberjar
$ taskset -c 0 java -Xmx16m -Xms16m -jar target/perm9-1.0.0-SNAPSHOT-standalone.jar -d 1500 -e 7 -i 500 -J 60 -G 2000
```

## License

Public Domain
