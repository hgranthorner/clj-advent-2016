(ns day-one-puzzle
  (:require [clojure.test :refer :all]
            [clojure.string :as str]))

(defn inspect [x]
  (println x)
  x)

(defn path-to-directions [path]
  (as-> (slurp path) n
    (str/split n #", ")
    (map #(hash-map 
            :direction (str (first %)) 
            :distance (Integer. (str/join (rest %)))) 
      n)))

(defn turn 
  [direction change]
  (case [direction change]
    ["U" "L"] "L"
    ["U" "R"] "R"
    ["R" "L"] "U"
    ["R" "R"] "D"
    ["D" "L"] "R"
    ["D" "R"] "L"
    ["L" "L"] "D"
    ["L" "R"] "U")
  )

(defn travel 
  [[x y pointing] {:keys [direction distance]}]
  (let [new-dir (turn pointing direction)]
    (case new-dir
      "U" [x (+ y distance) new-dir]
      "R" [(+ x distance) y new-dir]
      "D" [x (- y distance) new-dir]
      "L" [(- x distance) y new-dir])
    ))

(defn puzzle-one [path]
  (->> (path-to-directions path)
    (reduce travel [0 0 "U"])
    ((fn [[x y _]] (+ (Math/abs x) (Math/abs y)))))
  )

(defn find-dup
  [pos mps visited]
  (let [[x y new-dir] (travel pos (first mps))]
    (if (contains? visited [x y])
      [x y]
      (find-dup [x y new-dir] (rest mps) (conj i)))))

(defn to-array-map
  [posns]
  (to-array-map posns (array-map))
  [[[x y] & rem] arrmap]
  (if (contains? arrmap [x y])
    (to-array-map rem (update arrmap [x y] inc))
    (to-array-map rem (conj arrmap {[x y] 0}))))

(defn puzzle-two [path]
  "
  For each direction, travel that direction. Then check and see if it already exists in the set.
  If it does, return that position. Otherwise continue to the next direction.
  "
  (->> (path-to-directions path)
    (reductions travel [0 0 "U"])
    ))

(comment
  (puzzle-two "src/day-one/puzzle.txt")
  (puzzle-one "src/day-one/sample.txt")
  (puzzle-one "src/day-one/puzzle.txt")
  (some inspect {:a 2 :b 3})
  (+ -161 -145)
  )

(deftest test-puzzles
  (is (= (puzzle-one "src/day-one/sample.txt") 12))
  (is (= (puzzle-one "src/day-one/puzzle.txt") 332)))