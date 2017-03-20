(defproject gcbench "1.0.0-SNAPSHOT"
  :description "ubmltongroup clojure gcbench port"
  :dependencies [[org.clojure/clojure "1.4.0"]
  		 [org.clojure/tools.cli "0.3.5"]
                 [metrics-clojure-jvm "2.8.0"]]
  :main gcbench.core
  :jvm-opts ["-Xmx512m" "-server"] 
)
