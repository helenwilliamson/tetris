(ns tetris.core
    (:require [reagent.core :as reagent :refer [atom]]))

(enable-console-print!)

(def height 200)

(defonce app-state (atom [{:id 3 :x 20 :y 100 :height 20 :width 100}
                          {:id 2 :x 0 :y 40 :height 20 :width 100}
                          {:id 1 :x 100 :y 0 :height 20 :width 100}]))

(defn add-gravity 
  [state] 
  (map #(assoc % :y (+ 5 (:y %))) state))

(defn overlaps
  [first second]
  (let [first-bottom-right (hash-map :x (+ (:width first) (:x first)) :y (+ (:height first) (:y first)))
        second-bottom-right (hash-map :x (+ (:width second) (:x second)) :y (+ (:height second) (:y second)))]
    (not (or (>= (:x first) (:x second-bottom-right)) 
             (>= (:x second) (:x first-bottom-right))
             (>= (:y first) (:y second-bottom-right))
             (>= (:y second) (:y first-bottom-right))))))

(defn is-valid-world
  [state new-piece]
  (let [at-bottom (< height (+ (:height new-piece) (:y new-piece)))
        pieces-overlap (not= 0 (count (filter #(overlaps new-piece %1) state)))]
    (not (or at-bottom pieces-overlap))))

(defn drop-pieces 
  [state]
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

(defonce gravity (js/setInterval #(swap! app-state drop-pieces) 300))

(defn horizontal-rectangle
  [{id :id x :x y :y width :width height :height}]
  [:rect {:width width :height height :x x :y y :key id}])

(defn hello-world []
  [:div
   [:h3 "Tetris"]
   [:svg {:width 200 :height height}
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
