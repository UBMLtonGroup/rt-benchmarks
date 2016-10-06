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
