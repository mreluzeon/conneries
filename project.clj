(defproject conneries "0.1.0-pre-alpha"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.10.0"]
                 [org.blancas/kern "1.1.0"]]
  :main ^:skip-aot conneries.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
