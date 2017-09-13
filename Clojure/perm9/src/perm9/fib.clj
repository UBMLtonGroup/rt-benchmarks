(ns perm9.fib
    (:use [perm9.memstats])
   (:require
       [hara.concurrent.ova :refer :all]
    )
  )

(defn fib [max]
  (loop [res [0 1]]
          (if (>= (count res) max)
            res
            (recur (conj res (+' (inc (last res)) (dec (last (butlast res))))))))
)

;; 
(defn compute-thread-helper
  [compute-depth id niter compute-sleep debug oo]
  (if (> niter 0)
    (do
      (-> oo (append! [0 id niter (System/currentTimeMillis) (heap-used)]) (<<))
      (fib compute-depth)
      (-> oo (append! [1 id niter (System/currentTimeMillis) (heap-used)]) (<<))
      (Thread/sleep compute-sleep)
      (recur compute-depth id (- niter 1) compute-sleep debug oo)
     )

    (print-stats "compute" oo)

    )
  )

(defn compute-thread 
  "1. collect start time
   2. call compute-thread-helper which will iterate and:
       a. run computation
       b. repeat for N iters
   4. collect stop time
   5. output delta time with id"
  [compute-depth id niter compute-sleep debug]
  do 
     (compute-thread-helper compute-depth id niter compute-sleep debug (ova []))
)

(defn make-compute-threads [num-threads compute-depth niter compute-sleep debug]
  (if (> num-threads 0) 
       (dotimes [i num-threads] (.start (Thread. (fn [] (compute-thread compute-depth i niter compute-sleep debug)))))
  )
)


