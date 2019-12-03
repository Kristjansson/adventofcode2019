(ns adventofcode2019.p1
  (:require
    [adventofcode2019.core :as core]))


(comment ;; part 1
  (->> (core/read-lines "resources/01/1.txt")
       (map #(Integer/parseInt %))
       (map #(- (Math/floor (/ % 3)) 2))
       (reduce +)))


(defn fuel-cost
  [weight]
  (loop [curr 0
         mass weight]
    (let [required-fuel (- (Math/floor (/ mass 3)) 2)]
      (if (< 0 required-fuel)
        (recur
          (+ curr required-fuel)
          required-fuel)
        curr))))


(comment ;; part 2
  (->> (core/read-lines "resources/01/1.txt")
       (map #(Integer/parseInt %))
       (map fuel-cost)
       (reduce +)))
