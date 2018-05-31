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
  (let [at-bottom (< height (+ (:height new-piece) (:y new-piece)))
        pieces-overlap (not= 0 (count (filter #(overlaps new-piece %1) state)))]
    (not (or at-bottom pieces-overlap))))

(defn drop-piece
  [state]
  (reduce (fn
            [current-state piece]
            (let [new-piece (assoc piece :y (+ 25 (:y piece)))
                  new-state (conj current-state new-piece)]
              (if (is-valid-world current-state new-piece)
                new-state
                (conj current-state piece))))
          [] state))

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

(defn move-piece
  [state move]
  (println "move-piece" move)
  state)

(defn update-game
  [state]
  (let [new-state (drop-piece state)]
    (if (= state new-state)
      (js/setTimeout #(swap! game add-piece) 50))
    new-state))

(defonce gravity (reset! game-updater (js/setInterval #(swap! game update-game) 500)))

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

(def arrow-keys {37 :left 39 :right 40 :down})

(defn handle-keyboard-input
  [event]
  (let [key (.-keyCode event)]
    (if (contains? arrow-keys key)
      (swap! game move-piece (arrow-keys key)))))

(reagent/render-component [tetris]
                          (. js/document (getElementById "app")))
(swap! game drop-piece)
(swap! game add-piece)
(js/document.addEventListener "keydown" handle-keyboard-input)

(defn on-js-reload []
  ;; optionally touch your app-state to force rerendering depending on
  ;; your application
  ;; (swap! app-state update-in [:__figwheel_counter] inc)
)
