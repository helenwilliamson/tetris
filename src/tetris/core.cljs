(ns tetris.core
    (:require [reagent.core :as reagent :refer [atom]]
              [clojure.string :as str]))

(enable-console-print!)

(def height 300)
(def width 200)

(def colours ["red" "lime" "yellow" "aqua" "fuchsia" "blue"])
(def piece-type [:rectangle :square])

(defonce game (atom []))
(defonce status (atom ""))
(defonce game-updater (atom 0))

(defn update-values
  [m f & args]
  (reduce (fn [r [k v]] (assoc r k (apply f v args))) {} m))

(defn overlaps
  [{first-blocks :blocks} {second-blocks :blocks}]

  (let [first-values (set (vals first-blocks))
        second-values (set (vals second-blocks))]
    (println "first values" first-values)
    (println "second values" second-values)
    (some #(contains? second-values %1) first-values)))

(defn is-valid-world
  [state new-piece]
  (let [at-bottom-edge (some #(< height (+ 25 (:y %1))) (vals (:blocks new-piece)))
        at-left-edge (some #(< (:x %1) 0) (vals (:blocks new-piece)))
        at-right-edge (some #(> (+ 25 (:x %1)) width) (vals (:blocks new-piece)))
        pieces-overlap (not= 0 (count (filter #(overlaps new-piece %1) state)))]
    (not (or at-bottom-edge at-left-edge at-right-edge pieces-overlap))))

(defn apply-direction
  [piece direction]
  (let [blocks (:blocks piece)]
    (cond
     (= direction :down) (assoc piece :blocks (update-values blocks (fn [coord] (assoc coord :y (+ (:y coord) 25)))))
     (= direction :left) (assoc piece :blocks (update-values blocks (fn [coord] (assoc coord :x (- (:x coord) 25)))))
     (= direction :right) (assoc piece :blocks (update-values blocks (fn [coord] (assoc coord :x (+ (:x coord) 25)))))
     :else piece)))

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
   (= piece-type :rectangle) {:x x :y 0 :height 25 :width 100 :colour colour :blocks {id {:x x :y 0} (+ 1 id) {:x (+ 25 x) :y 0} (+ 2 id) {:x (+ 50 x) :y 0} (+ 3 id) {:x (+ 75 x) :y 0}}}
   (= piece-type :square) {:x x :y 0 :height 50 :width 50 :colour colour :blocks {id {:x x :y 0} (+ 1 id) {:x (+ 25 x) :y 0} (+ 2 id) {:x x :y 25} (+ 3 id) {:x (+ 25 x) :y 25}}}))

(defn add-piece
  [state]
  (let [random-colour (select-random colours)
        random-piece-type (select-random piece-type)
        possible-x (vec (range 0 (- width 100) 25))
        random-x (select-random possible-x)
        new-piece (make-piece (* 4 (count state)) random-x random-colour random-piece-type)]
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

(defn rectangle
  [block colour]
  (let [id (first block)
        {x :x y :y} (nth block 1)]
    [:rect {:width 25 :height 25 :x x :y y :key id :fill colour :stroke "black" :stroke-width 1}]))

(defn tetris []
  [:div
   [:h3 "Tetris"]
   [:svg {:width width :height height :style {:border "1px solid"}}
    (for [piece @game]
      (for [block (:blocks piece)]
        (rectangle block (:colour piece))))]
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
