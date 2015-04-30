(defproject vif-navigator-libs "0.1.0-SNAPSHOT"
            :description "Parser lib for vif navigator"
            :global-vars {*warn-on-reflection* true}
            :url "http://github"
            :dependencies [[org.clojure/clojure "1.6.0"]
                           [org.jsoup/jsoup "1.8.2" :use-resources true]
                           ]
            :source-paths ["src/clojure" "src"]
            :main ^:skip-aot ru.vif.model.parser
            :target-path "target/%s"
            :profiles {:uberjar {:aot :all}}
            :aliases {"build" ["install"]}
            )
