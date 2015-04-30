(ns ru.vif.model.parser
  (:require [ru.vif.model.jsoup-parser :as jp])
  (:import (java.io InputStream)
           (org.jsoup.nodes Node Document)))
(import org.jsoup.Jsoup)
(import ru.vif.model.jsoup_parser.tree-entry)


(defn trim-tree-by-depth [[first-entry & tree] ^long depth]
  "Обрезает дерево до заданной глубины"
  (time (reverse (reduce
                   (fn [list ^tree-entry entry]
                     (let [[^tree-entry prev & rest] list
                           ^int cur-depth (:depth entry)]
                       (if (>= cur-depth depth)
                         (conj rest (update-in prev [:child-count] inc))
                         (conj rest prev entry)
                         )
                       )
                     ) [first-entry] tree)
                 ))
  )

(defn calc-sub-depth [tree ^long depth]
  "Вычисляет количество дочерних элементов узла дерева"
  (loop [[^tree-entry next & rest :as full-list] tree
         count 0
         ]
    (if (some? next)
      (let [^long next-depth (:depth next)]
        (if (> next-depth depth)
          (recur rest (inc count))
          [count full-list]
          )
        )
      [count rest]
      )
    )
  )

(defn trim-tree-by-depth2 [tree ^long depth]
  "Обрезает дерево до заданной глубины - однопроходный алгоритм"
  (let [dec-depth (dec depth)]
    (loop [result []
           [entry & rest] tree
           ]
      (if (nil? entry)
        result
        (let [cur-depth (:depth entry)]
          (if (= cur-depth dec-depth)
            ;calc count
            (let [[count rest-rest] (calc-sub-depth rest dec-depth)]
              ;(println "child-count=" count "rest-rest" rest-rest)
              (recur (conj result (assoc entry :child-count count)) rest-rest))
            ;simple add
            (recur (conj result entry) rest)
            )
          )
        )
      )
    )
  )

(defn parse-tree-internal [^Document doc]
  "Разбирает документ и генерирует дерево"
  (jp/parse-element (time (.body doc)))
  )

(defn parse-tree
  "Генерирует jsoup модель и разбирает ее"
  ([^InputStream is
    ^String charset
    ^String baseUri]
   (parse-tree-internal (Jsoup/parse is charset baseUri))
    )

  ([^String html-body
    ^String baseUri]
   (parse-tree-internal (Jsoup/parse html-body baseUri))
    )
  )

(defn download-parse-tree
  "Выкачивает страницу, генерирует jsoup модель и разбирает ее"
  ([^String url
    ^long timeout
    ]
   (parse-tree-internal
     (-> (Jsoup/connect url)
         (.timeout timeout)
         .get
         )
     )
    )
  )



(defn find-element [tree ^String link]
  "находит в дереве узел с заданной ссылкой на сообщение, возвращает sequence из оставщися узлов"
  (drop-while #(not= link (:link %)) tree
              )
  )

(def find-one-element
  "находит в дереве узел с заданной ссылкой на сообщение, возвращает найденный элемент"
  (comp first find-element)
  )

(defn has-childs [tree ^String link]
  "Проверяет, если ли дочерние узлы у заданного узла"
  (let [elements (find-element tree link)]
    (if-let [[check-entry next-entry & rest] elements]
      (and (some? next-entry) (> (:depth next-entry) (:depth check-entry)))
      false
      )
    )
  )

(defn sub-tree [tree ^String link ^long depth]
  "Возвращает поддерево заданного узла, заданной глубины и с пересчитаными глубинами внутри поддерева"
  (let [[found-entry & rest] (find-element tree link)]
    (if (nil? found-entry)
      nil
      (trim-tree-by-depth
        (map
          #(update-in % [:depth] - (:depth found-entry))
          (cons found-entry (take-while #(< (:depth found-entry) (:depth %)) rest))
          )
        depth
        )
      )
    )
  )

(defn parent [tree ^String link]
  "Возвращает parent узел"
  (let [[found-entry & rest] (find-element (reverse tree) link)]
    (first (filter #(< (:depth %) (:depth found-entry)) rest)
           )
    )
  )

