# gcbench

This program was written to mimic the phase structure that has been conjectured for a class of application programs for which garbage collection may represent a significant fraction of the execution time.

[Generational garbage collectors perform extremely well on the early phases of this benchmark, but are likely to perform worse than non-generational collectors on the last phase or two.]

(http://www.larcenists.org/Twobit/benchmarks2.temp.html)

In our version we allow for multiple heap mutation threads and multiple computation threads to assess the impact of garbage collection on computation periodicity.

## Usage

```lein run -- -h```

or to compile into a jar

```lein uberjar
java -jar target/gcbench-1.0.0-SNAPSHOT-standalone.jar -h```

## License

Public Domain
