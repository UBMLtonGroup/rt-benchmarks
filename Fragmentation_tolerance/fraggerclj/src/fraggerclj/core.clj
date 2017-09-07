(ns fraggerclj.core
  (:gen-class))

(use 'ova.core)
(use 'clojure.pprint)

(defn map-every-nth [f coll n]
  (map-indexed #(if (zero? (mod (inc %1) n)) (f %2) %2) coll))

(defn fragmentArray
  [arr elem]
  ;;(<< (!! arr even? nil))
  ;;(map-every-nth (fn [n] (.set arr n 444)) arr 2)
  (doseq [n (range elem)
          :when (even? n)]
  (.set arr n nil))

)

(defn allocateArray 
  [kArraySize]
  ;;( ova (range kArraySize))
  (ova [kArraySize])
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

;17208402

(defn -main
  "I don't do a whole lot."
  [& args]
  (def arrlist (java.util.ArrayList.))
 
 (comment (def counter (atom 0))
  (while true
      (do 
       (def xxx (object-array [@counter]))
       (.add arrlist xxx)
        (swap! counter inc)
        (println @counter)
      )
   ))
 ;512358 
 ;9230095 
  (def elem 500000)
  (doseq [n (range elem)]
    (def xxx (object-array [n]))
    (.add arrlist xxx)
   ; (println n)
   )
  ;;(println "Done")
  ;;(pprint (into [] arrlist))
  (fragmentArray arrlist elem)
  (println (-> (java.lang.Runtime/getRuntime) (.freeMemory) ))
 ;; (pprint (into [] arrlist))
  (def start (+ elem 5))
  (def stop (+ start (int (/ elem 1.115))))
  (def begin (System/currentTimeMillis)) 
  (def yyy (object-array (range start stop)))
  ;;(println (-> (java.lang.Runtime/getRuntime) (.freeMemory) ))
  (def stop (System/currentTimeMillis))
  (println (- stop begin))
  ;;(def varia (.get arrlist 10000))
)

