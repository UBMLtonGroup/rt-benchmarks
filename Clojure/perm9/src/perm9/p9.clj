(ns perm9.p9
    (:use [perm9.memstats])
)

;; 
(defn gc-thread-helper
  [digits id niter debug]
  (if (> niter 0)
    (do
      (println (format "gc:start:%d:%d:%d" id niter (System/currentTimeMillis)))
      (comment "perm9 function call here, pass digits param as 'N' from Java which is hardcoded to 9... ")
      (println (format "gc:stop:%d:%d:%d" id niter (System/currentTimeMillis))) 
      (gc-thread-helper digits id (- niter 1) debug))
    )
  )

(defn gc-thread 
  "1. collect start time
   2. call gc-thread-helper which will iterate and:
      a. call perm9 with specified digits param
   3. collect stop time
   4. output delta time with id"
  [digits id niter debug]
  do 
     (gc-thread-helper digits id niter debug)
)

(defn make-gc-threads [num-threads digits niter debug]
  (if (> num-threads 0) 
       (dotimes [i num-threads] (.start (Thread. (fn [] (gc-thread digits i niter debug)))))
  )
)


