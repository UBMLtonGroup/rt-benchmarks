(defproject gcbench "1.0.0-SNAPSHOT"
  :description "ubmltongroup clojure gcbench port"
  :dependencies [[org.clojure/clojure   "1.5.1"]
  		         [org.clojure/tools.cli "0.3.5"]
                 [metrics-clojure-jvm   "2.8.0"]
                 [im.chit/hara "2.5.0"]
                 [im.chit/hara.concurrent.ova "2.5.10"]
                ]
                  :profiles {:dev {:dependencies [[midje "1.6.0"]]
                   :plugins [[lein-midje "3.1.3"]]}}
  :main gcbench.core
  :aot [gcbench.core]
  :jvm-opts ["-Xmx512m" "-server"] 
)
