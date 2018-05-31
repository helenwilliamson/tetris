(ns tetris.core
    (:require [reagent.core :as reagent :refer [atom]]))

(enable-console-print!)

(def height 200)

(defonce app-state (atom [{:id 1 :x 20 :y 100 :height 20 :width 100 :colour "red"}
                          {:id 2 :x 0 :y 40 :height 20 :width 100 :colour "blue"}
                          {:id 3 :x 100 :y 0 :height 20 :width 100 :colour "green"}]))

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
            (let [new-piece (assoc piece :y (+ 5 (:y piece)))
                  new-state (conj current-state new-piece)]
              (if (is-valid-world current-state new-piece)
                new-state
                (conj current-state piece))))
          [] state))

(defonce adder-interval (atom 0))

(defn add-piece
  [state]
  (let [new-piece {:id (count state) :x 50 :y 0 :height 20 :width 100 :colour "red"}]
    (if (is-valid-world state new-piece)
      (conj state new-piece)
     (do
       (js/clearInterval @adder-interval)
       state))))

(defonce gravity (js/setInterval #(swap! app-state drop-pieces) 500))

(defonce adder (reset! adder-interval (js/setInterval #(swap! app-state add-piece), 5000)))

(defn horizontal-rectangle
  [{id :id x :x y :y width :width height :height colour :colour}]
  [:rect {:width width :height height :x x :y y :key id :fill colour}])

(defn tetris []
  [:div
   [:h3 "Tetris"]
   [:svg {:width 200 :height height :style {:background "black" :border "1px solid"}}
    (for [piece @app-state]
      (horizontal-rectangle piece))]])

(reagent/render-component [tetris]
                          (. js/document (getElementById "app")))
(swap! app-state drop-pieces)

(defn on-js-reload []
  ;; optionally touch your app-state to force rerendering depending on
  ;; your application
  ;; (swap! app-state update-in [:__figwheel_counter] inc)
)
