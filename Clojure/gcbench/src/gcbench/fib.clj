(ns gcbench.fib)

(defn fib [max]
  (loop [res [0 1]]
          (if (>= (count res) max)
            res
            (recur (conj res (+' (inc (last res)) (dec (last (butlast res))))))))
)
