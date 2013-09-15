(defproject chess "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [compojure "1.1.5"]
                 [org.clojure/data.json "0.2.3"]
                 [ring/ring-core "1.2.0"]
                 [ring/ring-devel "1.2.0"]

                 [sandbar/sandbar "0.4.0-SNAPSHOT"]]
  :plugins [[lein-ring "0.8.5"]]
  :ring {:handler chess.web/handler}
  :profiles {:dev {:dependencies [[ring-mock "0.1.5"]
                                  [ring/ring-jetty-adapter "1.2.0"]]}}
  :main chess.core)
