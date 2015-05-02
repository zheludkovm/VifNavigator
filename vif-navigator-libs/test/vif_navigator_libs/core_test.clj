(ns vif-navigator-libs.core-test
  (:require [clojure.test :refer :all]
            [ru.vif.model.parser :refer :all]
            [ru.vif.model.jsoup-parser :refer :all]
            [clojure.java.io :as io]
            )
  (:import (java.io FileInputStream)
           (java.net URL)))

(import ru.vif.model.jsoup_parser.tree-entry)

(defn to-str-entry [^tree-entry entry]
  (str (:title entry) " - " (:post-author entry) " - " (:link entry) " " (:info entry))
  )

(defn pretty-print [tree]
  (doall
    (for [entry tree]
      (println
        (:depth entry) ":"
        (apply str (repeat (:depth entry) " "))
        (to-str-entry entry)
        ":"
        (:child-count entry)
        ":"
        (:is-visited entry)
        ":"
        (:non-visited-childs entry)
        ":"
        (:info entry)
        ":"
        (:post-time entry)

        )
      )
    )
  tree
  )

(deftest a-test
  (testing "parser"
    ;(pretty-print
    ;  ;(parse-tree (slurp "test-resources/tree-small2.html" :encoding "CP1251"))
    ;  (parse-tree (io/input-stream "test-resources/tree-small2.html") "CP1251" "tree.html")
    ;  )
    (let [
          visited-set #{"2681883.htm",
                        "2681661.htm"
                        }
          tree (parse-tree (io/input-stream "test-resources/tree.html") "CP1251" "tree.html")
          ;tree (parse-tree (io/input-stream "test-resources/tree-small2.html") "CP1251" "tree.html")
          ;tree (download-parse-tree "http://vif2ne.ru/nvk/forum/0/co/tree" 20000)
          trimmed-tree (sort-tree (trim-tree-by-depth 1 visited-set tree ))

          msg (extract-message-from-html (io/input-stream "test-resources/message2.html") "CP1251" "tree.html")

          ]

      (println "----------------------")
      ;(pretty-print tree)
      (println "----------------------")
      (pretty-print trimmed-tree)
      (println "----------------------")
      (pretty-print (sub-tree tree "2681579.htm" 3 visited-set))
      (println "----------------------")
      (println "----------------------")
      (println "subtree2" (sub-tree tree "2682351.htm" 3 visited-set))
      (pretty-print (sub-tree tree "2682351.htm" 3 visited-set))
      (println "----------------------")
      ;(println (parent tree "2672836.htm" ))
      ;(println "----------------------")
      ;(println msg)
      ;(println "has chhilds 1")
      ;(println (has-childs tree "2681896.htm"))
      ;(println "has chhilds 2")
      ;(println (has-childs tree "2681606.htm"))
      ;(println "has chhilds 3")
      ;(println (has-childs tree "2681418.htm"))


      ;(io/copy (io/input-stream "http://vif2ne.ru/nvk/forum/0/co/tree") (io/output-stream "test.html"))
      ;(io/copy (.openStream (URL. "http://vif2ne.ru/nvk/forum/0/co/tree")) (io/output-stream "test.html"))
      )
    )

  )
