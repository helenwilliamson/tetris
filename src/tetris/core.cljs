(ns tetris.core
    (:require [reagent.core :as reagent :refer [atom]]))

(enable-console-print!)

(def height 500)
(def width 500)

(def colours ["red" "lime" "yellow" "aqua" "fuchsia"])

(defonce game (atom []))
(defonce status (atom ""))

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
            (let [new-piece (assoc piece :y (+ 25 (:y piece)))
                  new-state (conj current-state new-piece)]
              (if (is-valid-world current-state new-piece)
                new-state
                (conj current-state piece))))
          [] state))

(defonce adder-interval (atom 0))

(defn add-piece
  [state]
  (let [random-colour (colours (rand-int (count colours)))
        possible-x (vec (range 0 (- width 100) 25))
        random-x (possible-x (rand-int (count possible-x)))
        new-piece {:id (count state) :x random-x :y 0 :height 25 :width 100 :colour random-colour}]
    (if (is-valid-world state new-piece)
      (conj state new-piece)
     (do
       (js/clearInterval @adder-interval)
       (reset! status "Game Over")
       state))))

(defonce gravity (js/setInterval #(swap! game drop-pieces) 500))

(defonce adder (reset! adder-interval (js/setInterval #(swap! game add-piece), 1000)))

(defn horizontal-rectangle
  [{id :id x :x y :y width :width height :height colour :colour}]
  [:rect {:width width :height height :x x :y y :key id :fill colour :stroke "black" :stroke-width 1}])

(defn tetris []
  [:div
   [:h3 "Tetris"]
   [:svg {:width width :height height :style {:border "1px solid"}}
    (for [piece @game]
      (horizontal-rectangle piece))]
   [:p {:style { :padding-left (- (/ width 2) 50)}}
    [:strong @status]]])

(reagent/render-component [tetris]
                          (. js/document (getElementById "app")))
(swap! game drop-pieces)
(swap! game add-piece)

(defn on-js-reload []
  ;; optionally touch your app-state to force rerendering depending on
  ;; your application
  ;; (swap! app-state update-in [:__figwheel_counter] inc)
)
