(ns gcbench.core)

;; http://macromancy.com/2014/04/09/data-structures-clojure-trees.html

(def gt? (comp pos? compare))

(def lt? (comp neg? compare))



(definterface INode
  (getLeft [])
  (getRight [])
  (lookup [k])
  (insert [k v]))

(deftype Node
  [^:volatile-mutable key
   ^:volatile-mutable val
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


(defn -main []
  "I can say 'Hello World'."
  (println "Hello, World!"))

(defn bst [& [k v]] (Node. k v nil nil))

