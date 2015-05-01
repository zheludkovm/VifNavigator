(ns ru.vif.file-tools
  (:require [neko.log :as log]
            [clojure.java.io :as io]
            )
  (:import (java.text SimpleDateFormat)
           (java.util Date)
           (android.app Activity)
           (android.content Context)
           (java.io FileReader BufferedReader BufferedWriter)
           )
  )



(def MAX-DAYS 20)

(def dt
  (* MAX-DAYS (* 24 3600 1000))
  )

(def FILE-NAME-FORMAT (SimpleDateFormat. "dd.MM.yyyy"))

(defn create-today-file-name []
  (.format FILE-NAME-FORMAT (Date.))
  )

(defn parse-file-name [^String file-name]
  (.parse FILE-NAME-FORMAT file-name)
  )

(defn is-old [^Date file-date]
  (> (- (System/currentTimeMillis) (.getTime file-date))
     dt
     )
  )



(defn serialize-visited [this visited-set]
  "Сохраняет в файл набор просмотренных ссылок"
  (if (not-empty visited-set)
    (let [file-name (create-today-file-name)
          context (.getApplicationContext this)]
      (log/d "save to file" file-name)
      (with-open [output (.openFileOutput context file-name Context/MODE_APPEND )
                  ^BufferedWriter writer (io/make-writer output {:append true})
                  ]
        (doall
          (for [link (seq visited-set)]
            (do
              (.write writer link)
              (.newLine writer)
              )
            )
          )
        (.flush writer)
        (.flush output)
        )
      )
    )
  )

(defn load-internal-file [file]
  "Загружает один файл или удаляет его, если он старый"
  (let [
        file-name (.getName file)
        file-date (parse-file-name file-name)
        ]
    (log/d "parse file " file)
    (if (or (nil? file-date) (is-old file-date))
      (do
        (log/d "delete file!" file-name)
        (.delete file)
        []
        )

      (with-open [^FileReader file-reader (FileReader. file)
                  ^BufferedReader buffered-reader (BufferedReader. file-reader)
                  ]
        (loop [result []]
          (if-let [link (.readLine buffered-reader)]
            (do
              (log/d "read" link)
              (recur (conj result link))
              )
            result
            )
          )
        )
      )
    )
  )

(defn load-visited [this]
  "Загружает просмотренные ссылки"
  (let [context (.getApplicationContext this)
        dir (.getFilesDir context)
        result (apply concat
                      (for [child-file (.listFiles dir)]
                        (load-internal-file child-file)
                        )
                      )
        ]
    (log/d "result" result)
    (set result)
    )
  )
