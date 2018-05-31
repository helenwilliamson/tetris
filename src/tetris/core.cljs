(ns tetris.core
    (:require [reagent.core :as reagent :refer [atom]]))

(enable-console-print!)

(println "This text is printed from src/tetris/core.cljs. Go ahead and edit it and see reloading in action.")

;; define your app data so that it doesn't get over-written on reload

(defonce app-state (atom [{:id 1 :x 100 :y 0 :height 20 :width 100}
                          {:id 2 :x 0 :y 40 :height 20 :width 100}
                          {:id 3 :x 20 :y 100 :height 20 :width 100}]))

(defn add-gravity 
  [state] 
  (map #(assoc % :y (+ 5 (:y %))) state))

(defn is-valid-world
  [state new-piece]
  (let [at-bottom (< 500 (+ (:height new-piece) (:y new-piece)))]
    (not at-bottom)))

(defn drop-pieces 
  [state]
  ;(println state)
  (reduce (fn 
            [current-state piece]
            ;(println "piece" piece)
            (let [new-piece (assoc piece :y (+ 5 (:y piece)))
                  new-state (conj current-state new-piece)]
              ;(println "new-piece" new-piece)
              ;(println "new-state" new-state)
              (if (is-valid-world current-state new-piece)
                new-state
                (conj current-state piece))))
          [] state))

(defonce gravity (js/setInterval #(swap! app-state drop-pieces) 50))

(defn horizontal-rectangle
  [{id :id x :x y :y width :width height :height}]
  [:rect {:width width :height height :x x :y y :key id}])

(defn hello-world []
  [:div
   [:h3 "Tetris"]
   [:svg {:width 200 :height 500}
    (for [piece @app-state]
      (horizontal-rectangle piece))]])

(reagent/render-component [hello-world]
                          (. js/document (getElementById "app")))
(swap! app-state drop-pieces)

(defn on-js-reload []
  ;; optionally touch your app-state to force rerendering depending on
  ;; your application
  ;; (swap! app-state update-in [:__figwheel_counter] inc)
)
