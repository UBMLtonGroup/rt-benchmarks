(defproject fraggerclj "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :main fraggerclj.core
  :aot [fraggerclj.core]
  :dependencies [[org.clojure/clojure "1.8.0"][im.chit/ova "1.0.1"]]
  :profiles {:minheap {:jvm-opts ["-Xloggc:gcclj" "-XX:+PrintGCDetails" "-XX:+UseSerialGC" "-XX:-UseGCOverheadLimit"]}})
