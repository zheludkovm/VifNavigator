(ns ru.vif.model.parser
  (:require [ru.vif.model.jsoup-parser :as jp])
  (:import (java.io InputStream)
           (org.jsoup.nodes Node Document)))
(import org.jsoup.Jsoup)
(import ru.vif.model.jsoup_parser.tree-entry)

(defn is-visited?
  (^Boolean [^tree-entry entry visited-set]
   (contains? visited-set (:link entry))
    )
  )

(defn update-visited
  [^Boolean is-visited ^tree-entry entry]
  (if is-visited
    (assoc entry :is-visited true)
    entry)
  )

(defn check-update-visited [^tree-entry entry visited-set]
  (update-visited (is-visited? entry visited-set) entry)
  )

(defn check-max-post-time [^Long parent-time ^Long check-time]
  (if (> check-time parent-time)
    check-time
    parent-time
    )
  )


(defn trim-tree-by-depth [^long depth visited-set [first-entry & tree]]
  "Обрезает дерево до заданной глубины"
  (time (if (nil? first-entry)
          []
          (reverse (reduce
                     (fn [list ^tree-entry entry]
                       (let [[^tree-entry prev & rest] list
                             ^int cur-depth (:depth entry)
                             ^long post-time (:post-time entry)
                             ^Boolean is-visited (is-visited? entry visited-set)
                             ]
                         (if (>= cur-depth depth)
                           (let [updated-prev (update-in prev [:child-count] inc)
                                 updated-prev1 (update-in updated-prev [:post-time] #(check-max-post-time % post-time))
                                 updated-prev-2 (if is-visited
                                                  updated-prev1
                                                  (update-in updated-prev1 [:non-visited-childs] inc)
                                                  )]
                             (conj rest updated-prev-2)
                             )
                           (conj rest prev (update-visited is-visited entry))
                           )
                         )
                       ) [(check-update-visited first-entry visited-set)] tree)
                   ))
        )
  )

;

(defn parse-tree-internal [^Document doc]
  "Разбирает документ и генерирует дерево"
  (jp/parse-element (time (.body doc)))
  )


(defn tree-starting-at [tree ^String link]
  "находит в дереве узел с заданной ссылкой на сообщение, возвращает sequence из оставщися узлов"
  (drop-while #(not= link (:link %)) tree
              )
  )

(def find-one-element
  "находит в дереве узел с заданной ссылкой на сообщение, возвращает найденный элемент"
  (comp first tree-starting-at)
  )

(defn parse-tree
  "Генерирует jsoup модель и разбирает ее"
  ([^InputStream is
    ^String charset
    ^String baseUri
    ]
   (parse-tree-internal (Jsoup/parse is charset baseUri))
    )

  ([^String html-body
    ^String baseUri]
   (parse-tree-internal (Jsoup/parse html-body baseUri))
    )
  )

(defn sub-tree [tree ^String link ^long depth visited-set]
  "Возвращает поддерево заданного узла, заданной глубины и с пересчитаными глубинами внутри поддерева"
  (let [[found-entry & rest] (tree-starting-at tree link)]
    (if (nil? found-entry)
      []
      (let [^long found-entry-depth (:depth found-entry)]
        (->>
          (take-while #(< found-entry-depth (:depth %)) rest)
          (map #(update-in % [:depth] - found-entry-depth))
          (trim-tree-by-depth depth visited-set)
          )
        )
      )
    )
  )

(defn calc-post-weight[^tree-entry entry]
  (let [^String title (:title entry)
        ^Long time (:post-time entry)
        ]
    (if (.contains title "<b>")
      (+ time 1000000000000)
      time
      )
    )
  )

(defn sort-tree [tree]
  (sort #(compare (calc-post-weight %2) (calc-post-weight %1)) tree)
  )