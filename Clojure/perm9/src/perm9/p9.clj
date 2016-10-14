(ns perm9.p9
    (:use [perm9.memstats])
)

(def perms (ref [[]]))	
(def x (ref []))

(defn tl [x] 
	(vec (rest x)))

(defn revloop [x0 n y]
	(if (zero? n) 
		y
		(revloop (tl x0) (- n 1) (into [(first x0)] y))))
		
(defn list_tail [x0 n]
	(if (zero? n)
		x0
		(list_tail (tl x0) (- n 1))))

(defn F [n] 
	(let [x0 (deref x)]
	(do 
		(dosync (ref-set x (revloop x0 n (list_tail x0 n))))
		(dosync (ref-set perms (into [(deref x)] (deref perms)))))))

(declare loop1)	

(defn P [n] 
	(if (> n 1)
		(loop1 (- n 1) n)
		()))
		
(defn loop1 [j n] 
	(if (zero? j) 
		(P (- n 1)) 
		(do 
			(P (- n 1)) 
			(F n) 
			(loop1 (- j 1) n))))	
		
(defn permutations [x0] 
	(do 
		(dosync (ref-set x (vec x0)))
		(dosync (ref-set perms [(deref x)]))
		(P (count x0)) 
		(deref perms)))

(defn one2n [n] 
	(letfn [(loop2 [n p] 
				(if (zero? n) 
					p 
					(loop2 (- n 1) (cons n p) )))]
	(loop2 n [])))
	
(defn sumlists [x0] 
	(letfn [(loop2 [sum x0] 
				(if (empty? (first x0)) 
					sum 
					(loop2 (apply + sum (first x0)) (rest x0))))]
	(loop2 0 x0)))
	
(defn factorial [n] 
	(if (= n 1) 
		1 
		(* n (factorial(- n 1)))))

(defn k_loop [k] 
	(if (zero? k) 
		(deref perms) 
		(do
			(dosync (ref-set perms (permutations (first (deref perms))))) 
			(k_loop (- k 1)))))

(defn perm9_benchmark [m n] 
	(do
		(dosync (ref-set perms (permutations (one2n n)))) 
		(k_loop m)
		(let [sum (sumlists (deref perms))
			  sum2 (/ (* (* n (+ n 1)) (factorial n)) 2)]
			(if (= sum sum2)
				()
				(print "*** Wrong result ***")))))

(defn printperms [] 
	(print (deref perms)))
		
;; 
(defn gc-thread-helper
  [digits id niter debug]
  (if (> niter 0)
    (do
      (println (format "gc:start:%d:%d:%d" id niter (System/currentTimeMillis)))
      (perm9_benchmark 5 9)
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


