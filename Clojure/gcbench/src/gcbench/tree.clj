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

(def ^{:dynamic true} long-lived-tree nil)
(def ^{:dynamic true} long-lived-array nil)

(defn exp [x n]
  (loop [acc 1 n n]
    (if (zero? n) acc
        (recur (* x acc) (dec n)))))

(defn make-tree [x] (exp 2 x))

(defn make-gc-threads [num-threads tree-depth warm-up debug]
  (if (> num-threads 0) 
    (do 
      (cond (true? warm-up)
         (do (
               cond (true? debug)
               (do
                 (println "warm up")
                 (binding [long-lived-tree (make-tree tree-depth)])
                 (binding [long-lived-array (make-array Integer/TYPE 1000)])
               )
             )
         )
         (dotimes [i num-threads] (.start (Thread. (fn [] (println i debug)))))
      )
    )
  )
)

;; http://macromancy.com/2014/04/09/data-structures-clojure-trees.html

(def gt? (comp pos? compare))

(def lt? (comp neg? compare))



(definterface INode
  (getLeft [])
  (getRight [])
  (lookup [k])
  (insert [k v]))

(deftype Node
  [^:volatile-mutable ^INode key
   ^:volatile-mutable ^INode val
   ^:volatile-mutable ^INode left
   ^:volatile-mutable ^INode right]

  INode
  (getLeft [_] left)
  (getRight [_] right)

  (insert [this k v]
    ;; establish a new node for insertion
    (let [n (Node. k v nil nil)]
      (cond
        ;; inserted key `k` is larger than root node's key
        (gt? k key) (if right             ;; if a right node
                      (.insert right k v) ;; recurse, else
                      (set! right n))     ;; set right to `n`

        ;; the inserted key `k` is less than the root node's key
        (lt? k key) (if left
                      (.insert left k v)
                      (set! left n)))))

   (lookup [this k]
    ;; check if current root's key matches search key `k`
    (if (= k key)
      val
      (cond
        ;; if both a non-nil right and `k` is greater than key
        (and (gt? k key) right) (.lookup right k)

        ;; if both a non-nil left and `k` is less than key
        (and (lt? k key) left) (.lookup left k)))))

