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

(def EXPAND_DEPTH 5)
(def MAIN_DEPTH 1)
(def LINK "link")
(def MSG "msg")


(defn calc-sub-tree [^String link ^Boolean remove-root]
  "Отрезает от дерева листья глубже чем EXPAND_DEPTH/MAIN_DEPTH а также считает количество дочерних сообщений"
  (let [full-data (:full-data @tree-data-store)
        sub-tree (if (some? link)
                   (parser/sub-tree full-data link EXPAND_DEPTH)
                   (parser/trim-tree-by-depth2 full-data MAIN_DEPTH)
                   )
        ]
    (if remove-root
      (drop 1 sub-tree)
      sub-tree
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
      (on-ui (launch-activity activity 'ru.vif.MsgActivity {MSG msg LINK link}))
      )
    )
  )

(defn full-reload [this]
  "Полная перегрузка всего дерева сообщений"
  (future
    (let [body (with-progress this R$string/process_loading_tree R$string/error_loading_tree download-vif-content)
          full-data (with-progress this R$string/process_tree R$string/error_process_tree #(parser/parse-tree body vif-url))
          tree-data (parser/trim-tree-by-depth2 full-data MAIN_DEPTH)]
      (on-ui
        (reset! tree-data-store {:tree-items tree-data
                                 :full-data  full-data
                                 :expand-log []
                                 })
        )
      )
    )
  )

;------------gui creation-----------------

(defn make-adapter [activity ^String link ^Boolean remove-root]
  "Создает адаптер для listview со списком сообщений"
  (let [show-message (partial show-message activity)
        ^String format-bold-black (str-res activity R$string/format_bold_black)
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
              color-function (if remove-root even? odd?)
              ]
          (config view
                  :backgroundResource (if (color-function position) R$color/odd R$color/even))
          (if (> depth-value 0)
            (config (find-view view ::depth) :text (repeat_void (:depth data)))
            )
          (config caption
                  :text (Html/fromHtml (str (:title data) (format format-bold-black (:post-author data))))
                  :tag link
                  )
          (config expandButton
                  :visibility (if (> child-count 0)
                                android.view.View/VISIBLE
                                android.view.View/GONE
                                )
                  :text (str "+" child-count)
                  :tag link
                  )
          )

        )
      (if (some? link) (ref (calc-sub-tree link remove-root)) tree-data-store)
      (if (some? link) identity :tree-items)
      )
    )
  )


(defactivity ru.vif.TreeActivity
             ;"Создает корневое activity со списком сообщений"
             :key :main
             :on-create
             (fn [^Activity this bundle]
               (on-ui
                 (set-content-view! this [:list-view {:id                 ::main-list-view
                                                      :adapter            (make-adapter this nil false)
                                                      :backgroundResource R$color/even
                                                      }])

                 (setup-action-bar this {
                                         :title              (str-res-html this R$string/main_title)
                                         :backgroundDrawable (.. this getResources (getDrawable R$color/odd))
                                         :display-options    :show-title
                                         })
                 )
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
             )

(defactivity ru.vif.MsgActivity
             ;"Создает activity с текстом cообщения и дочерними собщениями в дереве"
             :key :msg
             :on-create
             (fn [^Activity this bundle]
               (let [link (get-activity-param this LINK)
                     msg (get-activity-param this MSG)
                     tree-entry (find-one-element (:full-data @tree-data-store) link)
                     ]
                 (on-ui
                   (let [^ListView list-view (neko.ui/make-ui this [:list-view {:id                 ::msg-list-view
                                                                                :adapter            (make-adapter this link true)
                                                                                :backgroundResource R$color/even
                                                                                }])
                         ]
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

                     (set-content-view! this list-view)
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
             )

