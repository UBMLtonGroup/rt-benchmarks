
(def perms (ref [[]]))	
(def x (ref []))

(defn tl [x] (vec (rest x)))

(defn revloop [x0 n y]
	(if (zero? n) 
		y
		(recur (tl x0) (- n 1) (into [(first x0)] y))))
		
(defn list_tail [x0 n]
	(if (zero? n)
		x0
		(recur (tl x0) (- n 1))))

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
	(letfn [(loop [sum x0] 
				(if (empty? (first x0)) 
					sum 
					(recur (apply + sum (first x0)) (rest x0))))]
	(loop 0 x0)))()


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
		