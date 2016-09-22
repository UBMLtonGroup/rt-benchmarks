(deftype Node [left right])						;(def x (Node. (Tree. nil) (Tree. nil)))
												;retrieve val in left (.node (.left x))

(deftype Tree [node])							;(def x (Tree. nil)) for dummy

(defn make_empty_node [] (Node. (Tree. nil) (Tree. nil)))		;makes empty node properly 

(defn make_node [l r] (Node. l r))				;

(defn PrintDiagnostics [] 
	(let [lFreeMemory (.freeMemory (Runtime/getRuntime)) 
		  lTotalMemory (.totalMemory (Runtime/getRuntime))] 
	(print (str " Total memory available = " lTotalMemory " bytes, Free memory = " lFreeMemory " bytes"))))

(defn expt [m n] (loop [x 1 n n] (if (zero? n) x (recur (* m x) (dec n)))))
			
(defn TreeSize [i] (let [a (expt 2 (+ i 1))] (- a 1)))

(defn NumIters [i kStretchTreeDepth] (/ (* 2 (TreeSize kStretchTreeDepth)) (TreeSize i)))

(defn MakeTree [iDepth]
	(if (<= iDepth 0) 
		(make_empty_node)
		(make_node ((recur (- iDepth 1)) (recur (- iDepth 1))))))

(defn gcbench [kStretchTreeDepth]
	(let [kLongLivedTreeDepth (- kStretchTreeDepth 2)
		  kArraySize (* 4 (TreeSize kLongLivedTreeDepth))
		  kMinTreeDepth 4
		  kMaxTreeDepth kLongLivedTreeDepth] 
		(letfn [((Populate [iDepth #^Node node] 
					(let [lr (ref (.node (.left node)))
						  rr (ref (.node (.right node)))]
					(if (<= iDepth 0) 
						(false)
						(let [iDepth (- iDepth 1)] 
							((dosync (ref-set lr (make_empty_node))) 
							 (dosync (ref-set rr (make_empty_node)))
							 (Populate iDepth (deref lr)) 
							 (Populate iDepth (deref rr)))))))
				 
				 (TimeConstruction [depth] 
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
								((print "\tTop down construction took " (- tFinish tStart) " msecs"))))))))
				 (main [] 
				   ((print "Garbage Collector Test\n")
					(print (str " Stretching memory with a binary tree of depth " kStretchTreeDepth "\n"))
					(PrintDiagnostics ())
					(MakeTree kStretchTreeDepth)
					(print (str " Creating a long-lived binary tree of depth " kLongLivedTreeDepth "\n"))
					(let [longLivedTree (make_empty_node)]
						((Populate kLongLivedTreeDepth longLivedTree)
						(print (str " Creating a long-lived array of " kArraySize " inexact reals\n"))
						(let [arr (double-array kArraySize)]
							(letfn [(loop1 [i] 
										(if (< i (/ kArraySize 2)) 
											((aset-double arr i (/ 1 i)) (loop1 (+ i 1))) 
											())) 
									(loop2 [d] 
										(if (<= d kMaxTreeDepth) 
											((TimeConstruction d)(loop2 (+ d 2)))
											()))]
							   ((loop1 0)
								(PrintDiagnostics())
								(loop2 kMinTreeDepth)
								(if (or (= (.node longLivedTree) nil) (let [n (min (1000 (- (/ () 2) 1)))]
																	()))
									(print "Failed\n")
									()))))))
					(PrintDiagnostics ())))] 
				 (main()) )
		))

(defn main [] 
	(gcbench(18)))
	
(main)

