; http://macromancy.com/2014/04/09/data-structures-clojure-trees.html
;definterface INode and deftype Node using idea of nodes in link above
;added setLeft and setRight functions
;took out insert and lookup because they aren't needed
 
(definterface INode
  (getLeft [])
  (getRight [])
  (setLeft [])
  (setRight []))

(deftype Node
  [^:volatile-mutable ^INode key
   ^:volatile-mutable ^INode val
   ^:volatile-mutable ^INode left
   ^:volatile-mutable ^INode right]
   
  INode
  (getLeft [_] left)
  (getRight [_] right) 
  (setLeft [_] (set! left (Node. 0 0 nil nil)))
  (setRight [_] (set! right (Node. 0 0 nil nil))))


(defn PrintDiagnostics [] 
	(let [lFreeMemory (.freeMemory (Runtime/getRuntime)) 
		  lTotalMemory (.totalMemory (Runtime/getRuntime))] 
	(print (str " Total memory available = " lTotalMemory " bytes, Free memory = " lFreeMemory " bytes"))))

(defn expt [m n] 
	(loop [x 1 n n] 
		(if (zero? n) 
			x 
			(recur (* m x) (dec n)))))
			
(defn TreeSize [i] 
	(let [a (expt 2 (+ i 1))] 
		(- a 1)))

(defn NumIters [i kStretchTreeDepth] 
	(/ (* 2 (TreeSize kStretchTreeDepth)) (TreeSize i)))

(defn make_empty_node [] 
	(Node. 0 0 nil nil))
	
(defn make_node [l r] 
	(Node. 0 0 l r))

;MakeTree now works	
(defn MakeTree [iDepth] 
	(if (<= iDepth 0) 
		(make_empty_node) 
		(let [n (MakeTree (- iDepth 1))] 
			(make_node n n))))



			
;Populate still needs to be fixed
(defn Populate [iDepth #^Node node] 
	(if (<= iDepth 0) 
		false
		((.setRight node)
 		 (.setLeft node)
		 (let [i (- iDepth 1)] 
			((Populate i (.getLeft node)) 
			 (Populate i (.getRight node)))))))

;should be fine once populate works BUT should be checked	
(defn TimeConstruction [depth kStretchTreeDepth] 
	(let [iNumIters (NumIters depth kStretchTreeDepth)] 
		((print (str "Creating " iNumIters " trees of depth " depth))
		(let [tStart (System/currentTimeMillis)]
			((letfn [loop [i]
						(if (< i iNumIters) 
							((Populate depth make_empty_node) (loop [(+ i 1)])) 
							())]
					(loop 0)) 
			(let [tFinish (System/currentTimeMillis)] 
					(print "\tTop down construction took " (- tFinish tStart) " msecs"))))
		(let [tStart (System/currentTimeMillis)]
			((letfn [loop [i]
						((if (< i iNumIters) 
							((MakeTree depth) (loop (+ i 1))) 
							()) )] 
				(loop 0))
			(let [tFinish (System/currentTimeMillis)]
				((print "\tBottom up construction took " (- tFinish tStart) " msecs"))))))))
