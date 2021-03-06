(ns gcbench.tree
    (:use [gcbench.memstats])
    (:require
       [hara.concurrent.ova :refer :all]
    )
)

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

;; tree definition

(definterface INode
  (getLeft [])
  (getRight [])
  (setLeft [l])
  (setRight [r]))

(deftype Node
  [^:volatile-mutable ^INode key
   ^:volatile-mutable ^INode val
   ^:volatile-mutable ^INode left
   ^:volatile-mutable ^INode right]
  INode
  (getLeft [_] left)
  (getRight [_] right)
  (setLeft [_ l] (set! left l))
  (setRight [_ r] (set! right r))
)

(deftype Tree [node])

(defn make-empty-node [] 
        (Node. 0 0 nil nil))

(defn make-node [l r]
  (Node. 0 0 l r))


;; bottom up
 
(defn make-tree-bottom-up
  "Create a btree of given depth. Build is bottom up."
  [iDepth]
  (if (<= iDepth 0) 
    (make-empty-node)
    (make-node (make-tree-bottom-up (- iDepth 1))
               (make-tree-bottom-up (- iDepth 1))
               )))

;; top down

(defn make-tree-top-down
  "Create a btree of given depth. Build is top down."
  [iDepth thisNode]
  (if (<= iDepth 0)
    (let [iDepth (- iDepth 1)]
      (do 
         (.setLeft thisNode (make-empty-node))
         (.setRight thisNode (make-empty-node))
         (make-tree-top-down iDepth (.getLeft thisNode))
         (make-tree-top-down iDepth (.getRight thisNode))))))

;; 
(defn gc-thread-helper
  [tree-depth id niter gc-sleep debug oo]
  (if (> niter 0)
    (do
      (-> oo (append! [0 id niter (System/currentTimeMillis) (heap-used)]) (<<))

      (make-tree-bottom-up tree-depth)
      (make-tree-top-down tree-depth make-empty-node)

      (-> oo (append! [1 id niter (System/currentTimeMillis) (heap-used)]) (<<))

      (Thread/sleep gc-sleep)
      (recur tree-depth id (- niter 1) gc-sleep debug oo))

    (print-stats "gc" oo)
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
  [tree-depth id niter gc-sleep debug]
  do 
     (gc-thread-helper tree-depth id niter gc-sleep debug (ova []))
)

(defn make-gc-threads [num-threads tree-depth niter warm-up gc-sleep gc-delay debug]
  (if (> num-threads 0) 
    (do
      (if (true? warm-up)
         (do 
               (if (true? debug) (println "warm up"))
               (update-state 'long-lived-tree' (make-tree-top-down tree-depth make-empty-node))
               (update-state 'long-lived-array' (make-array Double/TYPE 500000))
         )
       )
       (dotimes [i num-threads] (.start (Thread. (fn [] (gc-thread tree-depth i niter gc-sleep debug)))))
    )
  )
)


