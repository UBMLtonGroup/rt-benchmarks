

(defn revloop [x n y]
	(if (zero? n) 
		(y)
		(recur (rest x) (- n 1) (cons (first x) y))))
		
(defn list_tail [x n]
	(if (zero? n)
		(x)
		(recur (rest x) (- n 1))))

(defn F [n x perms] 
	(let [tx (deref x) tp (deref perms)]
		((dosync (ref-set x (revloop (tx (n) (list_tail tx n)))))
		(dosync (ref-set perms (cons tx tp))))))
		
(defn permutations [x0] 
	(let [x  (ref x0)
		   perms (ref [x0])]
		(letfn [(P [n x perms] 
					(if (> n 1) 
						(letfn [loop [j] 
									(if (zero? j) 
										(P (- n 1)) 
										((P (- n 1)) (F n x perms) (loop (- j 1))))]
							(loop (- n 1))) 
						()))]
			(P (count(deref x)) (deref perms)))))

(defn sumlists [x] 
	(letfn [(loop [sum x] 
				(if (empty? (first x)) 
					sum 
					(recur (apply + sum (first x)) (rest x) )))]
	(loop 0 x)))
		
(def perms (ref [[]]))

(defn one2n [n] 
	(letfn [(loop [n p] 
				(if (zero? n) 
					p 
					(recur (- n 1) (cons n p) )))]
	(loop n [])))

(defn factorial [n] 
	(if (= n 1) 
		1 
		(* n (factorial(- n 1)))))
		
(defn perm9_benchmark [m n]
	((str m "perm" n) 
	(1)
	(dosync(ref-set perms ))
	))
		
(defn main [] 
	(perm9_benchmark(5 9))

(print "Done\n")