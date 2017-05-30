(ns fraggerclj.core
  (:gen-class))

(use 'ova.core)

(defn fragmentArray
  [arr]
(<< (!! arr even? nil))
)

(defn allocateArray 
  [kArraySize]
  ( ova (range kArraySize))
)

(defn allocateArray2 
  [kArraySize]
  (def sz (* kArraySize 3))
  ( ova (range sz (+ sz kArraySize) ))

)
(defn traverseArray
  [arr]
  (<< (map! arr (fn [n] (+ n 1))))
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
  (def kArraySize 100000)
  (def xxx (allocateArray kArraySize))
  (println "allocated large array")
  (def yyy (allocateArray2 (/ kArraySize 2)))
  (println "Allocated second array")
  ;;(time (allocateArray (/ kArraySize 2)))
 ;; (println  "Hello, World!")
  )

