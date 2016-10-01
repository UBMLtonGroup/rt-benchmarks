(ns gcbench.tree)

(comment "
  notes on implementation:
  
make-gc-threads num-threads tree-depth warm-up
   if num-threads > 0
     if warm-up
       make-tree tree-depth ;; long lived tree
       make-array 1000      ;; long lived array
     make the threads: gc-thread num-threads tree-depth
     
gc-thread id tree-depth
   collect start time
   make-tree tree-depth
   collect stop time
   output delta time with id
   recurse: gc-thread id tree-depth ;; ie forever

to get time in millisecs: (quot (System/currentTimeMillis) 1)
to get time in nanosecs: (quot (System/nanoTime) 1)
 " )

;; global state management 
;; really just stashing long-lived objects

(def state (atom {}))

(defn get-state [key]
  (@state key))

(defn update-state [key val]
  (swap! state assoc key val))

(defn exp [x n]
  (loop [acc 1 n n]
    (if (zero? n) acc
        (recur (* x acc) (dec n)))))

;; build tree bottom up

(definterface INode)
(deftype Node
  [^:volatile-mutable ^INode left
   ^:volatile-mutable ^INode right])
(deftype Tree [node])							                          ;(def x (Tree. nil)) for dummy
(defn make_empty_node [] (Node. (Tree. nil) (Tree. nil)))		;makes empty node properly
(defn make_node [l r] (Node. l r))		
(defn make-tree [iDepth]
	(if (<= iDepth 0) 
		(make_empty_node)
		(make_node ((recur (- iDepth 1)) (recur (- iDepth 1))))))

;; TODO
(defn gc-thread [tree-depth id debug]
  (println "gc-thread " tree-depth id debug))

(defn make-gc-threads [num-threads tree-depth warm-up debug]
  (if (> num-threads 0) 
    (do 
      (if (true? warm-up)
         (do 
               (if (true? debug) (println "warm up"))
               (update-state 'long-lived-tree' (make-tree tree-depth))
               (update-state 'long-lived-array' (make-array Integer/TYPE 1000))           
         )
       )
       (dotimes [i num-threads] (.start (Thread. (fn [] (gc-thread tree-depth i debug)))))
    )
  )
)


