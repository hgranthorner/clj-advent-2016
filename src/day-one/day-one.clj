(ns day-one
  (:require [clojure.test :refer :all]
            [clojure.string :as str]))

(defn inspect
  [x]
  (println x)
  x)

(defn string-to-directions
  "
  Receives a string in the shape 'L123'.
  Returns a map: {:turn L, :distance 123}.
  "
  [input]
  {:turn (str (first input))
   :distance (Integer. (str/join "" (rest input)))})

(comment
  (string-to-directions "L123"))

(defn parse-directions
  "
  Receives a ', ' delimited string of directions: 'L123'.
  Tranforms it into a list of maps with ids: {:turn 'L', :distance 123 :id 0}
  "
  [input]
  (as-> (str/split input #", ") x
    (map string-to-directions x)
    (map #(assoc %1 :id %2) x (iterate inc 1))))

(comment
  (determine-cardinalities (parse-directions "L123, R234, L456")))

(defn determine-cardinality
  "
  Takes an original cardinal direction, and a turn, and returns the new direction
  "
  [direction turn]
  (case [direction turn]
    ["N" "L"] "W"
    ["N" "R"] "E"
    ["E" "L"] "N"
    ["E" "R"] "S"
    ["S" "L"] "E"
    ["S" "R"] "W"
    ["W" "L"] "S"
    ["W" "R"] "N"))

(comment (= "W" (determine-cardinality "S" "R")))

(defn determine-cardinalities
  "
  Takes a list of directions, and adds a key indicating their cardinality to that map.
  "
  [directions]
  (filter #(contains? % :id) 
    (reduce 
      (fn [acc {:keys [turn] :as d}]
        (conj acc (assoc d :cardinality (determine-cardinality (get (last acc) :cardinality) turn))))
      [{:cardinality "N"}] directions)))

(defn follow-direction
  "
  Takes a position and a direction.
  Follows a direction, starting from a known position. Returns a position, i.e. `{:x 0 :y 1}`
  "
  [{:keys [x y]} {:keys [cardinality distance id]}]
  (case cardinality
    "N" {:x x :y (+ y distance) :id id}
    "S" {:x x :y (- y distance) :id id}
    "E" {:x (+ x distance) :y y :id id}
    "W" {:x (- x distance) :y y :id id}))

(defn puzzle-one
  "
  Receives a path. Tranforms a string into a number.
  String 
  -> List of maps (directions)
  -> List of maps (directions with cardinal direction)
  -> List of maps (coordinates)
  -> Last map (end coordinate)
  -> Number (abs val of x and y from coordinate)
  "
  [path]
   (as-> (parse-directions (slurp path)) x
     (determine-cardinalities x)
     (reduce follow-direction {:x 0 :y 0} x)
     (+ (Math/abs (get x :x)) (Math/abs (get x :y)))))

(defn already-seen?
  "
  Takes a position, and a map with positions as keys. Determines if that position is in the map
  "
  [{:keys [x y]} dict]
  (pos? 
    (count 
      (filter (fn [pos] (and (= x (get pos :x)) (= y (get pos :y)))) 
        (keys dict)))))

(already-seen? {:x 0 :y 0} {{:x 1 :y 0} 1})

(defn occurrence
  "
  Takes a position, and a list of positions. Sees how many times that x y coordinate exists.
  ")

(defn puzzle-two
  "
  Receives a path. Tranforms a string into a number.
  String
  -> List of maps (directions)
  -> List of maps (directions with cardinal direction)
  -> List of maps (coordinates with id)
  -> Reduce to map of coordinates -> # of times that x y has been seen
  -> Filter to only see coordinates with more than one time seen
  -> Sort by id
  "
  [path]
  (as-> (parse-directions (slurp path)) x
    (determine-cardinalities x)
    (reductions follow-direction {:x 0 :y 0 :id 0} x)))

(comment
  (puzzle-two "src/day-one/sample.txt")
  (puzzle-one "src/day-one/puzzle.txt")
  (def test-data [{:turn "L"} {:turn "L"} {:turn "R"} {:turn "L"}])
  (determine-cardinalities test-data)
  )