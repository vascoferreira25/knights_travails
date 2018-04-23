(ns knights-travails.core
  (:gen-class))

(defn board
  [side-length]
  ;; Create all the numbered cells from 0 to (* side-length side-length)
  (->> (range (* side-length side-length)) 
       ;; Split the values into parts with side-length elements
       (partition side-length)
       ;; Return a 2D map-vector
       (mapv vec)))

;; (board 8)

(defn empty-board
  [x y]
  (vec (for [height (range y)]
    (vec (for [width (range x)]
      0)))))

;; (assoc-in (empty-board 8 8) [0 2] 3)

(defn print-board
  [board]
  (for [row board]
    (println (str row "\n"))))

;; (print-board (empty-board 8 8))

(defn square-to-index
  [square-number]
  (let [y (int (/ square-number 8))
        x (int (- square-number (* y 8)))]
    [y x]))

;; (assoc-in (empty-board 8 8) (square-to-index 3) 5)

(defn draw-board
  ([path] (draw-board path (empty-board 8 8) 1))
  ([path board counter]
   (if-not (empty? path)
     (let [first-step (first path)
           coord (square-to-index first-step)]
       (recur (rest path) (assoc-in board coord counter) (inc counter)))
     (print-board board))))

;; (draw-board [3 2])

(defn knight-moves
  ([node]
   (knight-moves node nil)) ; Default parent to nil

  ([node parent]
   (let [offsets [6 10 15 17]
         pos (map #(+ node %) offsets) ; Just map over the offsets so you aren't writing them twice.
         neg (map #(- node %) offsets)
         ;; I'm reversing neg here so it matches your previous output. It seems valid without,
         ;; it just gives different answers.
         all-neighbours (vec (concat (reverse neg) pos)) ; Then stick the two together.
         possible-neighbours (vec (remove #(or (neg? %) (> % 63)) all-neighbours))]
     {:neighbours possible-neighbours :current-node node :parent-node parent})))

;; (knight-moves 35)

(defn nodes
  ([starting-node]
   (nodes starting-node nil)) ; Default to nil, just like before

  ([starting-node parent]
   (let [movements (knight-moves starting-node)]
     ;; You could use a full fn here instead of relying on the function macro
     ;;  if you want a better identifier than %
     (mapv #(knight-moves % (knight-moves starting-node parent))
           (:neighbours movements)))))

;; (nodes 35)

(defn backtracking
  "Return the parent location of each subsequent node in `path`."
  ([final-node] (backtracking final-node []))
  ([final-node path]
   (if (:parent-node final-node)
     (recur (:parent-node final-node) (merge path (:current-node final-node)))
     (vec (reverse (distinct path))))))

(defn find-path
  "Given a `starting-node`, find a path to the `goal-node`.
  It is not Dijkstra Algorithm because it doesn't calculate the distance
  between each point nor returns the shortest distance."
  ([starting-node goal-node]
   (find-path (knight-moves starting-node) goal-node []))
  ([starting-node goal-node path]
   ;; If not yet arrived at goal
   (if-not (= (:current-node starting-node) goal-node)
     ;; Create a new node with the current-node as parent and add it to path.
     ;; Iterate through all nodes to find the path.
     (let [new-node (nodes (:current-node starting-node) starting-node)
           path (concat path new-node)]
       (recur (first path) goal-node (vec (concat (rest path) new-node))))
     ;; If path found, return the path
     (backtracking starting-node))))

;; (find-path 27 28)

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (draw-board (find-path 27 28)))

(-main)
