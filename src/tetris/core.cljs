(ns tetris.core
    (:require [reagent.core :as reagent :refer [atom]]
              [clojure.string :as str]))

(enable-console-print!)

(def height 500)
(def width 500)

(def colours ["red" "lime" "yellow" "aqua" "fuchsia"])
(def piece-type [:rectangle :square])

(defonce game (atom []))
(defonce status (atom ""))
(defonce game-updater (atom 0))

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
  (let [at-bottom-edge (< height (+ (:height new-piece) (:y new-piece)))
        at-left-edge (< (:x new-piece) 0)
        at-right-edge (> (+ (:width new-piece) (:x new-piece)) width)
        pieces-overlap (not= 0 (count (filter #(overlaps new-piece %1) state)))]
    (not (or at-bottom-edge at-left-edge at-right-edge pieces-overlap))))

(defn apply-direction
  [piece direction]
  (cond
   (= direction :down) (assoc piece :y (+ (:y piece) 25))
   (= direction :left) (assoc piece :x (- (:x piece) 25))
   (= direction :right) (assoc piece :x (+ (:x piece) 25))
   :else piece))

(defn move-piece
  [state direction]
  (let [piece (last state)
        current-state (vec (take (- (count state) 1) state))
        new-piece (apply-direction piece direction)
        new-state (conj current-state new-piece)]
    (if (is-valid-world current-state new-piece)
      new-state
      (conj current-state piece))))

(defn rotate-piece
  [state]
  (let [piece (last state)
        current-state (vec (take (- (count state) 1) state))
        new-piece (assoc piece :width (:height piece) :height (:width piece))
        new-state (conj current-state new-piece)]
    (if (is-valid-world current-state new-piece)
      new-state
      (conj current-state piece))))

(defn select-random
  [data]
  (data (rand-int (count data))))

(defn make-piece
  [id x colour piece-type]
  (cond
   (= piece-type :rectangle) {:id id :x x :y 0 :height 25 :width 100 :colour colour}
   (= piece-type :square) {:id id :x x :y 0 :height 50 :width 50 :colour colour}))

(defn add-piece
  [state]
  (let [random-colour (select-random colours)
        random-piece-type (select-random piece-type)
        possible-x (vec (range 0 (- width 100) 25))
        random-x (select-random possible-x)
        new-piece (make-piece (count state) random-x random-colour random-piece-type)]
    (if (is-valid-world state new-piece)
      (conj state new-piece)
     (do
       (js/clearInterval @game-updater)
       (reset! status "Game Over")
       state))))

(defn update-game
  [state]
  (let [new-state (move-piece state :down)]
    (if (= state new-state)
      (js/setTimeout #(swap! game add-piece) 50))
    new-state))

(def movement-keys {37 :left 39 :right 40 :down})
(defn handle-keyboard-input
  [event]
  (let [key (.-keyCode event)]
    (cond
     (contains? movement-keys key) (swap! game move-piece (movement-keys key))
     (= 38 key) (swap! game rotate-piece))))

(defonce gravity (reset! game-updater (js/setInterval #(swap! game update-game) 500)))
(defonce adder (swap! game add-piece))

(defn horizontal-rectangle
  [{id :id x :x y :y width :width height :height colour :colour}]
  [:rect {:width width :height height :x x :y y :key id :fill colour :stroke "black" :stroke-width 1}])

(defn tetris []
  [:div
   [:h3 "Tetris"]
   [:svg {:width width :height height :style {:border "1px solid"}}
    (for [piece @game]
      (horizontal-rectangle piece))]
   [:p
    [:strong @status]]])

(reagent/render-component [tetris]
                          (. js/document (getElementById "app")))

(js/document.addEventListener "keydown" handle-keyboard-input)

(defn on-js-reload []
  ;; optionally touch your app-state to force rerendering depending on
  ;; your application
  ;; (swap! app-state update-in [:__figwheel_counter] inc)
)
