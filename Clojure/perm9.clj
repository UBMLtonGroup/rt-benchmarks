
(def perms (ref [[]]))	
(def x (ref []))

(defn tl [x] (vec (rest x)))

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
	(do (dosync (ref-set x (revloop x0 n (list_tail x0 n))))
	(dosync (ref-set perms (into [(deref x)] (deref perms)))))))

(declare loop1)	

(defn P [n] 
	(if (> n 1)
		(loop1 (- n 1) n)
		()))
		
(defn loop1 [j n] 
	(if (zero? j) 
		(P (- n 1)) 
		(do (P (- n 1)) (F n) (loop1 (- j 1) n))))	
		
(defn permutations [x0] 
	(do (dosync (ref-set x (vec x0)))
	(dosync (ref-set perms [(deref x)]))
	(P (count x0)) 
	(deref perms)))

(defn sumlists [x0] 
	(letfn [(loop2 [sum x0] 
				(if (empty? (first x0)) 
					sum 
					(loop2 (apply + sum (first x0)) (rest x0))))]
	(loop2 0 x0)))


(defn one2n [n] 
	(letfn [(loop2 [n p] 
				(if (zero? n) 
					p 
					(loop2 (- n 1) (cons n p) )))]
	(loop2 n [])))

(defn factorial [n] 
	(if (= n 1) 
		1 
		(* n (factorial(- n 1)))))
		