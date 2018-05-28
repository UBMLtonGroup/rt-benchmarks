(defproject perm9 "1.0.0-SNAPSHOT"
  :description "ubmltongroup clojure perm9 port"
  :dependencies [[org.clojure/clojure "1.4.0"]
           		   [org.clojure/tools.cli "0.3.5"]
                 [im.chit/hara "2.5.0"]
                 [im.chit/hara.concurrent.ova "2.5.10"]
                ]
  :profiles {:dev {:dependencies [[midje "1.6.0"]]
                   :plugins [[lein-midje "3.1.3"]]}}

  :main perm9.core
  :aot [perm9.core]
  :jvm-opts ["-Xmx512m" "-server"] 
)
