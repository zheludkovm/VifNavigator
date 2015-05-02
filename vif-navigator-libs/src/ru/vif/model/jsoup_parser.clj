(ns ru.vif.model.jsoup-parser
  (:import (org.jsoup.select Elements)
           (java.net URL)
           (java.io InputStream)
           (org.jsoup.nodes Node Document)))
(import org.jsoup.Jsoup)
(import java.util.Date)
(import org.jsoup.nodes.Element)
(import java.text.SimpleDateFormat)

(defrecord tree-entry [^long depth,
                       ^String title,
                       ^String link,
                       ^int child-count,
                       ^String post-author,
                       ^String info,
                       ^Boolean is-visited
                       ^long non-visited-childs
                       ^long post-time
                       ])

(defn element-tag
  (^String [^Element element]
   "Возвращает имя тега элемента JSOUP"
   (.getName (.tag element)))
  )

(defn sibling-seq [^Element element]
  "Возвращает sequence из элементов одного уровня (siblings) который начинается с заданного элемента"
  (->> (iterate (fn [^Element e] (.nextSibling e)) element)
       (take-while #(some? %))
       )
  )

(defn next-sibling [^Element element]
  "Обертка над методом .nextElementSibling"
  (.nextElementSibling element)
  )

(defn sibling-html [^Element element ^String tag]
  "Возвращает html тега соседа (sibling) заданного именем тега"
  (when-let [^Element found-element
             (->> (iterate next-sibling element)
                  (take-while #(some? %))
                  (filter #(= (element-tag %) tag))
                  first
                  )]
    (.html found-element)
    )
  )

(defn calc-depth [^Element element]
  "Вычисляет глубину элемента"
  (-> element
      .parents
      .size
      (- 6)
      (/ 2)
      )
  )

(def ^SimpleDateFormat TIME_FORMAT (SimpleDateFormat. "dd.MM.yyyy HH:mm:ss"))

(defn parse-time [^String info]
  (let[^Date date (.parse TIME_FORMAT info)]
    (.getTime date)
    )
  )

(defn parse-one-element [^Element element]
  "Разбирает один элемент jsoup, возвращает либо инициализированную tree-entity, либо строку hr, либо nil"
  (let [^String tagName (element-tag element)
        ^String href (.attr element "href")
        ]
    (cond
      ;ссылка
      (and (= tagName "a")
           (not (clojure.string/blank? href))
           (not (.contains href "javascript"))
           )
      (let [^int tree-depth (calc-depth element)
            ^Element next-element (.nextElementSibling element)
            ^Element next-next-element (.nextElementSibling next-element)
            ^String post-author (.html next-element)
            ^String info (.html next-next-element)
            ^long post-time (parse-time info)
            ]
        (tree-entry. tree-depth (.html element) href 0 post-author info false 0 post-time)
        ;(tree-entry. tree-depth (.html element) href 0 (sibling-html element "b") (sibling-html element "i"))
        )
      ;разделитель
      (= tagName "hr") :hr
      ;пропускаем
      :else nil
      )
    )
  )



(defn parse-element
  "Разбирает body, возвращает sequence из заполненных tree-entity"
  [^Element element]
  (->> element
       .getAllElements
       .iterator
       iterator-seq
       (keep #(parse-one-element %))
       (take-while #(not= % :hr))
       doall
       time
       )
  )

(defn extract-message [^Document doc]
  "Разбирает документ, извлекает сообщение пользователя"
  (let [^Element body (.body doc)]
    (.remove (.child body 0))
    (.remove (.child body 0))

    (->> body
         .getAllElements
         .iterator
         iterator-seq
         (drop-while #(not= (element-tag %) "hr"))
         (map (fn [^Element e](.remove e)))
         doall
         )
    (.html body)
    )
  )


(defn extract-message-from-html
  "Разбирает string или inputstream, извлекает сообщение пользователя"
  ([^InputStream is
    ^String charset
    ^String baseUri]
   (extract-message (Jsoup/parse is charset baseUri))
    )
  ([^String html-body
    ^String baseUri]
   (extract-message (Jsoup/parse html-body baseUri))
    )
  )
