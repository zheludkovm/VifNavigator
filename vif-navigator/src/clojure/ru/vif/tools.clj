(ns ru.vif.tools
  (:require [neko.activity :refer [defactivity set-content-view!]]
            [neko.debug :refer [*a safe-for-ui]]
            [neko.threading :refer [on-ui on-ui*]]
            [neko.resource :refer [import-all get-resource]]
            [neko.ui.menu :as menu]
            [neko.log :as log]
            [neko.notify :as notify]
            [neko.ui.adapters :refer [ref-adapter]]
            [neko.find-view :refer [find-view find-views]]
            [neko.ui :refer [config make-ui-element]]
            [neko.action-bar :refer [setup-action-bar]]
            [clojure.java.io :as io]

            [ru.vif.model.parser :as parser :refer :all]
            [ru.vif.http-client :as http :refer :all]
            [ru.vif.model.parser :as parser :refer :all]
            [ru.vif.model.jsoup-parser :as jsoup-parser :refer :all]
            )
  )

(neko.resource/import-all)

(import ru.vif.model.jsoup_parser.tree-entry)
(import android.content.Intent)
(import android.text.Html)
(import android.view.View)
(import android.text.method.ScrollingMovementMethod)
(import android.text.method.LinkMovementMethod)
(import android.graphics.Color)
(import android.graphics.drawable.ColorDrawable)
(import android.app.Activity)
(import android.content.Context)

(defn str-res
  (^String [^Activity this ^Integer res]
   "Загружает ресурсы android приолжения"
   (-> this
       .getResources
       (.getText res)
       )
    )
  )

(defn from-html [^String html]
  "Генерирует spinner"
  (Html/fromHtml html)
  )

(def str-res-html
  "загружает ресурс и генерирует spinner"
  (comp from-html str-res)
  )

(defn res-format
  (^String [this res & args]
   "загружает ресурс и применяет в качестве формата"
   (apply format (cons (str-res this res) args))
    )
  )

(def res-format-html
  "загружает ресурс, форматирует и преобразует результат как html"
  (comp from-html res-format)
  )

(defn repeat_void [depth]
  "генерит нужное количество отступов"
  (apply str (repeat (* 3 depth) " "))
  )


(defn launch-activity [a launch-activity params]
  "Запуск activity с набором параметров"
  (let [^Intent intent (Intent. a (resolve launch-activity))]
    (doall (for [[^String key ^String value] params]
             (.putExtra intent key value)
             )
           )
    (.startActivity a intent)
    )
  )

(defn get-activity-param [activity paramName]
  "получить именованый параметр activity"
  (.. activity getIntent (getStringExtra paramName))
  )

(defn with-progress [this message-res error-message-res function]
  "Отображает диалог с сообщением пока идет обработка в фооне, в случае возникновения ошибки - выдает длинный toast"
  (let [^String message (str-res this message-res)
        ^String error-message (str-res this error-message-res)
        dialog (atom nil)]
    (on-ui (let [pb (neko.ui/make-ui this [:progress-dialog
                                           {:progress-style :spinner
                                            :message        message}
                                           ])
                 ]
             (.show pb)
             (reset! dialog pb)
             )
           )
    (try
      (let [result (function)]
        (on-ui (.hide @dialog))
        result
        )
      (catch Exception e
        (log/d (str "caught exception: " (.getMessage e)))
        (on-ui
          (.hide @dialog)
          (notify/toast this error-message :long)
          )
        )
      )
    )
  )


