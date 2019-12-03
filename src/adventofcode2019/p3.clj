(ns adventofcode2019.p3
  (:require
   [adventofcode2019.core :as core]
   [clojure.string :as str]
   [clojure.set :as set]))


(defn path-step
  [[curr-x curr-y] direction distance]
  (case direction
    "R" (map vector (range (inc curr-x) (+ curr-x distance 1)) (repeat curr-y))
    "L" (map vector (range (dec curr-x) (- curr-x distance 1) -1) (repeat curr-y))

    "U" (map vector (repeat curr-x) (range (inc curr-y) (+ curr-y distance 1)))
    "D" (map vector (repeat curr-x) (range (dec curr-y) (- curr-y distance 1) -1))))


(defn commands->path
  [commands]
  (->> (str/split commands #",")
       (map (fn [command] [(subs command 0 1) (Integer/parseInt (subs command 1))]))
       (reduce
         (fn [path [direction distance]]
           (concat path (path-step (last path) direction distance)))
         [[0 0]])))


(comment ;;part 1
  (def path1 (->> (slurp "resources/03/1.txt")
                  (str/split-lines)
                  (first)
                  (commands->path)))


  (def path2 (->> (slurp "resources/03/1.txt")
                  (str/split-lines)
                  (second)
                  (commands->path)))


  (->> (set/intersection (set path1) (set path2))
       (map (fn [[x y]] (+ (Math/abs x) (Math/abs y))))
       (sort)
       (clojure.pprint/pprint)))


(defn index-path
  [path]
  (->> (map-indexed (fn [idx postition] {postition idx}) path)
       (apply merge-with min)))


(assert (= {[:a] 0, [:b] 1, [:c] 2}
           (index-path [[:a] [:b] [:c] [:b] [:a]])))


(comment ;;part 2
  (def path1-index (index-path path1))

  (def path2-index (index-path path2))

  (->> (set/intersection (set path1) (set path2))
       (map (fn [pos] (+ (path1-index pos) (path2-index pos))))
       (sort)
       (clojure.pprint/pprint)))
