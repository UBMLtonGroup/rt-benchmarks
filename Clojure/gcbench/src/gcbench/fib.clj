(ns gcbench.fib)

(defn fib [max]
  (loop [res [0 1]]
          (if (>= (count res) max)
            res
            (recur (conj res (+' (inc (last res)) (dec (last (butlast res))))))))
)

;; 
(defn compute-thread-helper
  [compute-depth niter compute-sleep debug]
  (if (> niter 0)
    (do(fib compute-depth)
       (Thread/sleep compute-sleep)
       (compute-thread-helper compute-depth (- niter 1) compute-sleep debug)
       )
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
     (println (format "compute:start:%d:%d" id (System/currentTimeMillis)))
     (compute-thread-helper compute-depth niter compute-sleep debug)
     (println (format "compute:stop:%d:%d" id (System/currentTimeMillis))) 
)

(defn make-compute-threads [num-threads compute-depth niter compute-sleep debug]
  (if (> num-threads 0) 
       (dotimes [i num-threads] (.start (Thread. (fn [] (compute-thread compute-depth i niter compute-sleep debug)))))
  )
)


