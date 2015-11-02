(ns sudoku
  (:require [clojure.set :as set]))

; An empty board
(def board identity)

; Numbers from 1 to 9. Valid values for a block/row/col
(def all-values #{1 2 3 4 5 6 7 8 9})

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (not (= 0 (value-at board coord))))

(defn row-values [board [row col]]
  (set (get board row)))

(defn col-values [board [row col]]
  (set (map (fn[x] (get x col)) board)))

(defn coord-pairs [coords]
  (for [x coords y coords] [x y]))

(defn upleft-corner [[row col]]
  [(- row (mod row 3)) (- col (mod col 3))])

(defn block-values [board coord]
  (let [ [top-x top-y] (upleft-corner coord) ]
    (set (for [x (range top-x (+ top-x 3))  y (range top-y (+ top-y 3)) ]
           (get-in board [x y])))))

(defn valid-values-for [board coord]
(cond 
 (has-value? board coord) #{}
 :else (set/difference all-values 
                       (col-values board coord) 
                       (row-values board coord) 
                       (block-values board coord))))

(defn filled? [board]
  (not (contains? (set (apply concat board)) 0)))

(defn rows [board]
  (reduce (fn [acc coll] (conj acc (set coll))) [] board))

; A helper for checking if all collections match
(defn contains-all? [values]
  (every? (fn [v] (= v all-values)) values))

(defn valid-rows? [board]
  (contains-all? (rows board)))

(defn cols [board]
  (for [column (range 9)] (col-values board [0 column])))

(defn valid-cols? [board]
  (contains-all? (cols board)))

(defn blocks [board]
  (vec (for [corner (coord-pairs (range 0 9 3))] 
         (block-values board corner))))

(defn valid-blocks? [board]
  (contains-all? (blocks board)))

(defn valid-solution? [board]
  (and (valid-blocks? board) (valid-rows? board) (valid-cols? board)))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn find-empty-point [board]
  (first (filter (fn [coord] (not (has-value? board coord))) 
                 (for [x (range 9) y (range 9)] [x y]))))

(defn solve [board]
  nil)
