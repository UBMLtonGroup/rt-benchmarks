(ns gcbench.memstats)

;; https://github.com/sjl/metrics-clojure/blob/master/docs/source/jvm.rst

;; https://groups.google.com/forum/#!topic/clojure/dzelKZrIoH4

(defmulti sizeof class) 
(defmethod sizeof Number ([_] 4)) ; or decide by size if you prefer 
(defmethod sizeof java.util.Collection 
  ([coll] 
    (reduce + 4 (map sizeof (seq coll))))) 
(defmethod sizeof clojure.lang.ISeq 
  ([coll] 
    (reduce + 4 (map sizeof (seq coll))))) 

(defn heap-used
  "Report a (inconsistent) snapshot of the heap memory used."
  []
  (let [runtime (Runtime/getRuntime)]
    (- (.totalMemory runtime) (.freeMemory runtime))))



(defn print-stats
  [t a]
  (dotimes [i (count a) ]
   (if (= (nth (nth a i) 0) 0)
       (println (format "%s:start:%d:%d:%d:%d" t
                 (nth (nth a i) 1)
                 (nth (nth a i) 2)
                 (nth (nth a i) 3)
                 (nth (nth a i) 4)
                 ))
       (println (format "%s:stop:%d:%d:%d:%d" t
                 (nth (nth a i) 1)
                 (nth (nth a i) 2)
                 (nth (nth a i) 3)
                 (nth (nth a i) 4)
                 ))
   )
  )
)



(comment "
;; duck types no long available:
;; with-in-reader migrated to https://clojuredocs.org/clojure.java.io
(defn gc []
  (dotimes [_ 4] (System/gc)))

(defn used-memory []
  (let [runtime (Runtime/getRuntime)]
    (gc)
    (- (.totalMemory runtime) (.freeMemory runtime))))

(defn measure [f]
  (let [before (used-memory)
        _ (def foo (with-in-reader f (read)))
        after (used-memory)]
    (- after before)))

")
