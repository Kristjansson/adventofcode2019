(ns adventofcode2019.p6
  (:require
    [adventofcode2019.core :as core]
    [clojure.string :as str]
    [clojure.set :as set]
    [clojure.walk :as walk]))


;;         G - H       J - K - L
;;        /           /
;; COM - B - C - D - E - F
;;                \
;;                 I
;; (+ 11,10,1,0,7,6,0,4,0,2,1)


(defn checksum
  [orbit-tree]
  (->> orbit-tree
       (walk/postwalk
         (fn [x]
           (if (keyword? x)
             0
             (assoc x 0 (reduce + (count (rest x)) (map first (rest x)))))))
       (flatten)
       (reduce +)))


(defn parse-orbit
  [orbit-str]
  (->> (str/split orbit-str #"\)")
       (mapv keyword)))


(defn build-tree
  [edges root]
  (let [children (edges root)]
    (vec
      (concat
        [root]
        (mapv #(build-tree edges %) children)))))


(assert (= 42
           (checksum
             (build-tree {:COM [:B]
                          :B [:G :C]
                          :G [:H]
                          :C [:D]
                          :D [:E :I]
                          :E [:J :F]
                          :J [:K]
                          :K [:L]} :COM))))


(comment ;; Part 1
  (def direct-orbits
    (->> (map parse-orbit (core/read-lines "resources/06/1.txt"))
         (group-by first)
         (map (fn [[root children]] [root (map second children)]))
         (into {})))

  (prn
    (checksum
      (build-tree direct-orbits :COM))))

(set/difference #{1 2 3} #{3 4 5})

(defn bfs
  [adjacency-matrix start destination]
  (loop [path-queue (list [start])
         visited #{start}]

    (when (empty? path-queue)
      (throw (ex-info "EMPTY QUEUE" {})))

    (let [curr-path (first path-queue)
          curr (last curr-path)
          neighbors (adjacency-matrix curr)
          new-neighbors (set/difference neighbors visited)]

      (if (contains? new-neighbors destination)
        (conj curr-path destination)
        (recur
          (concat (rest path-queue) (map (partial conj curr-path) new-neighbors))
          (set/union visited new-neighbors))))))


(comment ;; Part 2
  (def adjacency-matrix
    (->> (map parse-orbit (core/read-lines "resources/06/1.txt"))
         (mapcat (fn [orbit] [orbit (vec (reverse orbit))]))
         (group-by first)
         (map (fn [[root children]] [root (set (map second children))]))
         (into {})))

  (- (count (bfs adjacency-matrix :YOU :SAN)) 3)
  )

(time
  (checksum
    [:COM
     [:B
      [:G
       [:H]]
      [:C
       [:D
        [:I]
        [:E
         [:J
          [:K
           [:L]]]
         [:F]]]]]]))
