(ns ru.vif.main
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
            [ru.vif.tools :refer :all]
            [ru.vif.file-tools :refer :all]
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
(import android.widget.TextView)
(import android.widget.ListView)


(def tree-data-store
  (atom {:tree-items []
         :full-data  []
         }
        )
  )

(def visited-set
  (atom #{})
  )

(def new-visited-set
  (atom #{})
  )

(def EXPAND_DEPTH 5)
(def MAIN_DEPTH 1)
(def LINK "link")
(def MSG "msg")


(defn calc-sub-tree [^String link]
  "Отрезает от дерева листья глубже чем EXPAND_DEPTH/MAIN_DEPTH а также считает количество дочерних сообщений"
  (let [full-data (:full-data @tree-data-store)
        ]
    (log/d "show " link)
    (if (some? link)
      (parser/sub-tree full-data link EXPAND_DEPTH @visited-set)
      (parser/trim-tree-by-depth MAIN_DEPTH @visited-set full-data)
      )
    )
  )

(defn show-message [^Activity activity ^TextView caption]
  "Скачивает сообщение (с прогресс баром), разбирает его и запускает в окне просмотра"
  (future
    (let [link (.getTag caption)
          html (with-progress activity R$string/process_loading_message R$string/error_loading_message #(http/download-message link))
          msg (jsoup-parser/extract-message-from-html html link)
          ]
      (swap! visited-set conj link)
      (swap! new-visited-set conj link)
      (on-ui (launch-activity activity 'ru.vif.MsgActivity {MSG msg LINK link}))
      )
    )
  )

(defn full-reload [this]
  "Полная перегрузка всего дерева сообщений"
  (future
    (let [body (with-progress this R$string/process_loading_tree R$string/error_loading_tree download-vif-content)
          full-data (with-progress this R$string/process_tree R$string/error_process_tree #(parser/parse-tree body vif-url))
          tree-data (parser/trim-tree-by-depth MAIN_DEPTH @visited-set full-data)]
      (on-ui
        (reset! tree-data-store {:tree-items tree-data
                                 :full-data  full-data})
        )
      )
    )
  )

(defn refresh-adapter-data [list-view tree-data-store tree-data]
  "перегружает данные адаптера listview и по возможности восстанавлиает положение скролбара"
  (let [visible-position (.getFirstVisiblePosition list-view)]
    (on-ui (swap! tree-data-store assoc :tree-items tree-data)
           (.setSelection list-view visible-position)
           )
    )
  )

;------------gui creation-----------------

(defn make-adapter [activity ^String link data-store access-fn]
  "Создает адаптер для listview со списком сообщений"
  (let [show-message (partial show-message activity)
        ^String format-bold-black (str-res activity R$string/format_bold_black)
        ^String visited-mark (str-res activity R$string/visited_mark)
        ^String not-visited-mark (str-res activity R$string/not_visited_mark)
        ^String not-visited-count-format (str-res activity R$string/not_visited_count)
        ^String visited-count-zero-format (str-res activity R$string/visited_count_zero)
        ^String child-count-format (str-res activity R$string/child_count_format)
        ]
    (ref-adapter
      (fn [_]
        [:relative-layout {:id-holder true :layout-width :fill}
         [:text-view {:id                       ::depth
                      :layout-align-parent-left true
                      }]
         [:text-view {:id                 ::caption
                      :layout-to-right-of ::depth
                      :layout-to-left-of  ::expandButton
                      :min-lines          2
                      :on-click           show-message
                      }]
         [:button {:id                        ::expandButton
                   :layout-align-parent-right true
                   :layout-height             :wrap
                   :on-click                  show-message
                   }]
         ]
        )
      (fn [position view _ ^tree-entry data]
        (let [caption (find-view view ::caption)
              expandButton (find-view view ::expandButton)
              child-count (:child-count data)
              link (:link data)
              depth-value (:depth data)
              color-function (if (some? link) even? odd?)
              is-visited (:is-visited data)
              non-visited-childs (:non-visited-childs data)
              ]
          ;фон, четный и нечетный
          (config view
                  :backgroundResource (if (color-function position) R$color/odd R$color/even))
          ;отступы
          (if (> depth-value 0)
            (config (find-view view ::depth) :text (repeat_void (:depth data)))
            )
          ;текст
          (config caption
                  :text (Html/fromHtml (str (:title data)
                                            (format format-bold-black (:post-author data))
                                            (if is-visited visited-mark not-visited-mark)
                                            )
                                       )
                  :tag link
                  )
          ;кнопка
          (config expandButton
                  :visibility (if (> child-count 0)
                                android.view.View/VISIBLE
                                android.view.View/GONE
                                )
                  :text (Html/fromHtml
                          (str (if (> non-visited-childs 0)
                                 (format not-visited-count-format non-visited-childs)
                                 )
                               (format child-count-format child-count)
                               )
                          )
                  :tag link
                  )
          )

        )
      data-store
      access-fn
      )
    )
  )


(defactivity ru.vif.TreeActivity
             ;"Создает корневое activity со списком сообщений"
             :key :main
             :on-create
             (fn [^Activity this bundle]
               (reset! visited-set (load-visited this))
               (on-ui
                 (set-content-view! this [:list-view {:id                 ::main-list-view
                                                      :adapter            (make-adapter this nil tree-data-store :tree-items)
                                                      :backgroundResource R$color/even
                                                      }])

                 (setup-action-bar this {
                                         :title              (str-res-html this R$string/main_title)
                                         :backgroundDrawable (.. this getResources (getDrawable R$color/odd))
                                         :display-options    :show-title
                                         })
                 )
               (full-reload this)
               )
             :on-create-options-menu
             (fn [this menu]
               (safe-for-ui
                 (menu/make-menu
                   menu [[:item {
                                 :icon           R$drawable/redraw
                                 :show-as-action :always
                                 :on-click       (fn [_] (full-reload this))}]
                         ]
                   )
                 )
               )
             :on-resume
             (fn [this]
               (refresh-adapter-data
                 (find-view this ::main-list-view)
                 tree-data-store
                 (parser/trim-tree-by-depth MAIN_DEPTH @visited-set (:full-data @tree-data-store))
                 )
               )

             :on-stop
             (fn [this]
               (log/d "stop!")
               (serialize-visited this @new-visited-set)
               (reset! new-visited-set #{})
               )
             )



(defactivity ru.vif.MsgActivity
             ;"Создает activity с текстом cообщения и дочерними собщениями в дереве"
             :key :msg
             :state (atom {:tree-items []})
             :on-create
             (fn [^Activity this bundle]
               (let [link (get-activity-param this LINK)
                     msg (get-activity-param this MSG)
                     tree-entry (find-one-element (:full-data @tree-data-store) link)
                     state (.state this)
                     ;internal-data (calc-sub-tree link)
                     ]
                 ;(reset! state {:tree-items internal-data})
                 (on-ui
                   (let [list-view-tree [:list-view {:id                 ::msg-list-view
                                                     :adapter            (make-adapter this link state :tree-items)
                                                     :backgroundResource R$color/even
                                                     }
                                         ]]
                     (set-content-view! this list-view-tree)
                     (let [list-view (find-view this ::msg-list-view)]
                       (.addHeaderView list-view (neko.ui/make-ui this [:text-view {:text               (res-format-html this R$string/message_format
                                                                                                                         (:title tree-entry)
                                                                                                                         (:info tree-entry)
                                                                                                                         msg
                                                                                                                         )
                                                                                    :max-lines          10000
                                                                                    :backgroundResource R$color/even
                                                                                    :movement-method    (LinkMovementMethod.)
                                                                                    }
                                                                        ]))
                       )

                     )

                   (setup-action-bar this {
                                           :title              (res-format-html this R$string/title_format (:post-author tree-entry))
                                           :backgroundDrawable (.. this getResources (getDrawable R$color/odd))
                                           :display-options    [:home-as-up :show-title]
                                           })
                   )
                 )
               )
             :on-options-item-selected
             (fn [^Activity this item]
               (.finish this)
               )
             :on-resume
             (fn [this]
               (refresh-adapter-data
                 (find-view this ::msg-list-view)
                 (.state this)
                 (calc-sub-tree (get-activity-param this LINK))
                 )
               )

             )

