(ns skaro.game)

;;; rules

(defn move [[x y] input]
  (case input
    "up" [x (dec y)]
    "down" [x (inc y)]
    "left" [(dec x) y]
    "right" [(inc x) y]))

(defn allowed? [{:keys [width height]} [x y]]
  (and x y (<= 0 x width) (<= 0 y height)))

(defn collision? [obstacles position]
  (> (count (filter (partial = position) obstacles)) 1))

(defn get-collisions [enemies obstacles]
  (filter (partial collision? obstacles) enemies))

(defn killed? [{:keys [player enemies piles]}]
  (some (partial = player) (concat enemies piles)))

;;; drawing

(defn place [marker rows position]
  (assoc-in rows (reverse position) marker))

(defn draw-board [{:keys [height width player enemies piles] :as board}]
  (let [rows (into [] (repeat height (into [] (repeat width '_))))
        rows (place 'O rows player)
        rows (reduce (partial place 'M) rows enemies)
        rows (reduce (partial place 'X) rows piles)]
    (mapv prn rows)))

;;; game loop

(defn move-player [{:keys [player width height] :as board} input]
  (if (= "teleport" input)
    (assoc board :player [(rand-int width) (rand-int height)])
    (let [new-position (move player input)]
      (if (allowed? board new-position)
        (assoc board :player new-position)
        board))))

(defn move-enemy [[px py] [x y]]
  (let [dx (- px x)
        dy (- py y)]
    (if (> (Math/abs dx) (Math/abs dy))
        [((if (pos? dx) inc dec) x) y]
        [x ((if (pos? dy) inc dec) y)])))

(defn move-enemies [{:keys [enemies player] :as board}]
  (update-in board [:enemies] (partial map (partial move-enemy player))))

(defn collisions [{:keys [enemies piles] :as board}]
  (let [collisions (get-collisions enemies (concat enemies piles))]
    (-> (update-in board [:piles] into collisions)
        (update-in [:enemies] (partial remove (set collisions))))))

(def round (comp collisions move-enemies move-player))

(defn play [board input]
  (cond (killed? board)
        (println "You died.")
        (= input "quit")
        (println "Bye.")
        (empty? (board :enemies))
        (println "You won. Nice job.")
        :else
        (recur (doto (round board input) draw-board) (read-line))))

;;; setup

(defn make-board [width height enemies]
  (doto {:width width :height height :piles #{}
         :enemies (for [_ (range enemies)]
                    [(rand-int width) (rand-int height)])
         :player [(rand-int width) (rand-int height)]} draw-board))

(defn -main [& args]
  (play (make-board 10 10 4) (read-line)))
