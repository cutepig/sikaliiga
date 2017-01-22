(ns sikaliiga.core
  (:require [reagent.core :as reagent :refer [atom]]
            [sikaliiga.ui.core :as ui]))

(enable-console-print!)

(println "This text is printed from src/sikaliiga/core.cljs. Go ahead and edit it and see reloading in action.")

(reagent/render-component [ui/app-view ui/app-state]
                          (. js/document (getElementById "app")))

(defn on-js-reload []
  ; optionally touch your app-state to force rerendering depending on
  ; your application
  (swap! ui/app-state update-in [:__figwheel_counter] inc))
