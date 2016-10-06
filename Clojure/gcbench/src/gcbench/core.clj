
(ns gcbench.core
  (:require [clojure.string :as string] 
            ;; https://github.com/clojure/tools.cli
            [clojure.tools.cli :refer [parse-opts]])
  (:use [gcbench.tree] [gcbench.memstats])
  (:gen-class))





(def cli-options
  ;; An option with a required argument
  [["-t" "--compute-threads NUM" "Compute Threads"
    :default 1
    :parse-fn #(Integer/parseInt %)
    :validate [#(< 0 % 0x10000) "Must be a number between 0 and 65536"]]
   ["-d" "--compute-depth NUM" "Compute Depth"
    :default 37
    :parse-fn #(Integer/parseInt %)
    :validate [#(< 1 % 0x10000) "Must be a number between 0 and 65536"]]
   ["-i" "--iterations NUM" "Iterations"
    :default 10
    :parse-fn #(Integer/parseInt %)
    :validate [#(< 1 % 0x10000) "Must be a number between 0 and 65536"]]
   ["-s" "--compute-sleep NUM" "Compute Sleep"
    :default 1
    :parse-fn #(Integer/parseInt %)
    :validate [#(< 0 % 0x10000) "Must be a number between 0 and 65536"]]
   ["-g" "--gc-threads NUM" "GC Threads"
    :default 1
    :parse-fn #(Integer/parseInt %)
    :validate [#(< 0 % 0x10000) "Must be a number between 0 and 65536"]]
   ["-e" "--tree-depth NUM" "Maximum tree depth to allocate"
    :default 10
    :parse-fn #(Integer/parseInt %)
    :validate [#(< 1 % 0x10000) "Must be a number between 0 and 65536"]]
   ["-m" "--maxheap NUM" "Maximum heap to allocate (in MB)"
    :default 4
    :parse-fn #(Integer/parseInt %)
    :validate [#(< 0 % 0x10000) "Must be a number between 0 and 65536"]]
   ;; A non-idempotent option
   ["-S" "--gc-stats" "Print GC stats"]
   ["-D" "--debug" "Tell us what you are doing."]
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
      errors (exit 1 (error-msg errors)))
  (make-tree (:tree-depth options))
  (make-gc-threads (:gc-threads options)
                   (:tree-depth options)
                   (:iterations options)
                   true 
                   (:debug options))
  )
)
  
