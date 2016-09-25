(ns gcbench.core
  (:require [clojure.string :as string] [clojure.tools.cli :refer [parse-opts]])
  (:gen-class))


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

(defn bst [& [k v]] (Node. k v nil nil))

;; ./yourprogram -ct <#> -ci <#> -cs <#> -gt <#> -ts <#> -m <#> -s -h

(def cli-options
  ;; An option with a required argument
  [["-ct" "--compute-threads NUM" "Compute Threads"
    :default 1
    :parse-fn #(Integer/parseInt %)
    :validate [#(< 0 % 0x10000) "Must be a number between 0 and 65536"]]
   ["-cd" "--compute-depth NUM" "Compute Depth"
    :default 37
    :parse-fn #(Integer/parseInt %)
    :validate [#(< 1 % 0x10000) "Must be a number between 0 and 65536"]]
   ["-ci" "--compute-iterations NUM" "Compute Iterations"
    :default 10
    :parse-fn #(Integer/parseInt %)
    :validate [#(< 1 % 0x10000) "Must be a number between 0 and 65536"]]
   ["-cs" "--compute-sleep NUM" "Compute Sleep"
    :default 1
    :parse-fn #(Integer/parseInt %)
    :validate [#(< 0 % 0x10000) "Must be a number between 0 and 65536"]]
   ["-gt" "--gc-threads NUM" "GC Threads"
    :default 1
    :parse-fn #(Integer/parseInt %)
    :validate [#(< 0 % 0x10000) "Must be a number between 0 and 65536"]]
   ["-td" "--tree-depth NUM" "Maximum tree depth to allocate"
    :default 50
    :parse-fn #(Integer/parseInt %)
    :validate [#(< 1 % 0x10000) "Must be a number between 0 and 65536"]]
   ["-m" "--maxheap NUM" "Maximum heap to allocate (in MB)"
    :default 4
    :parse-fn #(Integer/parseInt %)
    :validate [#(< 0 % 0x10000) "Must be a number between 0 and 65536"]]
   ;; A non-idempotent option
   ["-s" "--gc-stats" "Print GC stats"]
   ;; A boolean option defaulting to nil
   ["-h" "--help"]])

(defn usage [options-summary]
  (->> ["Clojure MT/GC Bench"
        ""
        "Usage: lein run -- [options]"
        ""
        "Options:"
        options-summary
       ]
       (string/join \newline)))

(defn error-msg [errors]
  (str "The following errors occurred while parsing your command:\n\n"
       (string/join \newline errors)))

(defn exit [status msg]
  (println msg)
  (System/exit status))


(defn -main [& args]
  (let [{:keys [options arguments errors summary]} (parse-opts args cli-options)]
    ;; Handle help and error conditions
    (cond
      (:help options) (exit 0 (usage summary))
      (not= (count arguments) 1) (exit 1 (usage summary))
      errors (exit 1 (error-msg errors)))
  (println "Hello, World!")))
  
