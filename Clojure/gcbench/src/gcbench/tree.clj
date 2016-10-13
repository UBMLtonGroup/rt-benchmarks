(ns gcbench.tree
    (:use [gcbench.memstats])
)

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
   recurse: gc-thread id tree-depth

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
  [^:volatile-mutable ^INode key
   ^:volatile-mutable ^INode val
   ^:volatile-mutable ^INode left
   ^:volatile-mutable ^INode right])

(deftype Tree [node])
(defn make_empty_node [] 
        (Node. 0 0 nil nil))
(defn make_node2 [l r depth] 
  (do (println "makenode at depth " depth)
        (Node. 0 0 l r)))
(defn make-tree2 [iDepth]
        (if (<= iDepth 0) 
                (make_empty_node) 
                (let [n (make-tree2 (- iDepth 1))] 
                        (make_node2 n n iDepth))))

(defn make_node [l r]
  (Node. 0 0 l r))


;; bottom up
 
(defn make-tree-bottom-up
  "Create a btree of given depth. Build is bottom up."
  [iDepth]
  (if (<= iDepth 0) 
    (make_empty_node)
    (make_node (make-tree-bottom-up (- iDepth 1))
               (make-tree-bottom-up (- iDepth 1))
               )))

;; top down
(defn make-tree 
  "Create a btree of given depth. Build is top down."
  [iDepth]
  (if (<= iDepth 0)
      (make_empty_node)
      (make_node (make-tree (- iDepth 1))
                 (make-tree (- iDepth 1)))))

;; 
(defn gc-thread-helper
  [tree-depth id niter debug]
  (if (> niter 0)
    (do
      (println (format "gc:start:%d:%d:%d" id niter (System/currentTimeMillis)))
      (make-tree-bottom-up tree-depth)
      (println (format "gc:stop:%d:%d:%d" id niter (System/currentTimeMillis))) 
      (gc-thread-helper tree-depth id (- niter 1) debug))
    )
  )

(defn gc-thread 
  "1. collect start time
   2. call gc-thread-helper which will iterate and:
       a. make-tree tree-depth
       b. destroy tree
       c. repeat for N iters
   4. collect stop time
   5. output delta time with id"
  [tree-depth id niter debug]
  do 
     (gc-thread-helper tree-depth id niter debug)
)

(defn make-gc-threads [num-threads tree-depth niter warm-up debug]
  (if (> num-threads 0) 
    (do 
      (if (true? warm-up)
         (do 
               (if (true? debug) (println "warm up"))
               (update-state 'long-lived-tree' (make-tree tree-depth))
               (update-state 'long-lived-array' (make-array Double/TYPE 500000))
         )
       )
       (dotimes [i num-threads] (.start (Thread. (fn [] (gc-thread tree-depth i niter debug)))))
    )
  )
)


