;;This program allocates one large array and defragments half and allocates another array half the
;;size of original array

(ns fraggerclj.core
  (:gen-class))

(use 'ova.core)
;(use 'clojure.pprint)

(defn map-every-nth [f coll n]
  (map-indexed #(if (zero? (mod (inc %1) n)) (f %2) %2) coll))

(defn fragmentArray
  [arr]
  (<< (!! arr even? nil))
  ;;(map-every-nth (fn [n] (aset arr n nil)) arr 2)
)

(defn allocateArray 
  [kArraySize]
  ( ova (range kArraySize))
  ;;( object-array (range kArraySize))
)

(defn allocateArray2 
  [kArraySize]
  (def sz (* kArraySize 3))
  ( ova (range sz (+ sz kArraySize) ))
  ;;( object-array (range sz (+ sz kArraySize) ))

)
(defn traverseArray
  [arr]
  (<< (map! arr (fn [n] (+ n 1))))
  ;;(map-every-nth inc arr 1)
)

;;(defn allocateArray 
;;  [kArraySize]
;;  (def arr (ova (range kArraySize)))
;;  (fragmentArray arr)
;;  (println arr)
;;  )

(defn -main
  "I don't do a whole lot."
  [& args]
  (def kArraySize 1000000)
  (def xxx (allocateArray kArraySize))
  ;;(print "allocated large array\n")
  (traverseArray xxx) ;;Traverse to avoid optimizations of any sort
  ;;(pprint xxx)
  (fragmentArray xxx)
  ;;(pprint xxx)
  (def yyy (time (allocateArray2 410000)))
  ;;(println "Allocated second array")
  ;;(time (allocateArray (/ kArraySize 2)))
 ;; (println  "Hello, World!")
  )

