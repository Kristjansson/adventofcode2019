(ns adventofcode2019.p2
  (:require
    [adventofcode2019.core :as core]
    [clojure.string :as str]))


(defn read-int-array
  [file-name]
  (->> (str/split (str/trim (slurp file-name)) #"\,")
       (mapv #(Integer/parseInt %))))


(defn add
  [memory in1 in2 out]
  (assoc memory out (+ (get memory in1)
                       (get memory in2))))


(defn mul
  [memory in1 in2 out]
  (assoc memory out (* (get memory in1)
                       (get memory in2))))


(defn process-op
  [memory op-ptr]
  (case (get memory op-ptr)
    1 [(add memory
            (get memory (+ 1 op-ptr))
            (get memory (+ 2 op-ptr))
            (get memory (+ 3 op-ptr)))
       (+ op-ptr 4)]
    2 [(mul memory
            (get memory (+ 1 op-ptr))
            (get memory (+ 2 op-ptr))
            (get memory (+ 3 op-ptr)))
       (+ op-ptr 4)]
    (do
      (prn "saw exit code" (get memory op-ptr))
      [memory -1])))


(defn run-program
  [program]
  (loop [memory program
         op-ptr 0]
    (if (<= 0 op-ptr)
      (let [[memory' op-ptr'] (process-op memory op-ptr)]
        (recur memory' op-ptr'))
      memory)))


(comment ;; Part 1
  (let [memory (-> (read-int-array "resources/02/1.txt")
                   (assoc 1 12)
                   (assoc 2 2))]
    (get (run-program memory) 0)))


(->> (partition-all 4 (read-int-array "resources/02/1.txt"))
     (map-indexed (fn [idx val] [(* 4 idx) val]))
     (clojure.pprint/pprint))



(defn sym-add
  [memory in1 in2 out]
  (assoc memory out (str "("
                         (get memory in1)
                         " + "
                         (get memory in2)
                         ")")))


(defn sym-mul
  [memory in1 in2 out]
  (assoc memory out (str "("
                         (get memory in1)
                         " * "
                         (get memory in2)
                         ")")))


(defn sym-process-op
  [memory op-ptr]
  (case (get memory op-ptr)
    1 [(sym-add memory
            (get memory (+ 1 op-ptr))
            (get memory (+ 2 op-ptr))
            (get memory (+ 3 op-ptr)))
       (+ op-ptr 4)]
    2 [(sym-mul memory
            (get memory (+ 1 op-ptr))
            (get memory (+ 2 op-ptr))
            (get memory (+ 3 op-ptr)))
       (+ op-ptr 4)]
    (do
      (prn "saw exit code" (get memory op-ptr))
      [memory -1])))


(defn sym-run-program
  [program]
  (loop [memory program
         op-ptr 0]
    (if (<= 0 op-ptr)
      (let [[memory' op-ptr'] (sym-process-op memory op-ptr)]
        (recur memory' op-ptr'))
      memory)))


(comment ;; Part 2
  (let [memory (-> (read-int-array "resources/02/1.txt")
                   (assoc 1 "x")
                   (assoc 2 "y"))]
    (prn (get (sym-run-program memory) 0)))

  ;; 3 lines of python later:
  ;; In [1]: import sympy

  ;; In [2]: sympy.simplify("(1 + (((4 * (1 + ((4 * (((5 + (2 * (((((((((((((5 + (2 + (1 + ((((5 + (4 + (((2 * x) + 3) + 3))) + 4) * 3) + 5)))) + 2) + 4) * 2) + 1) * 4) + 2) + 1) + 2) + 5) + 2) * 4) + 2))) * 4) + 1)) * 3))) * 3) + y))")
  ;; Out[2]: 221184*x + y + 2880733

  (assert (= 76.0 (Math/floor (/ (- 19690720 2880733) 221184))))

  (assert (= 19690720 (+ (* 221184 76) 3 2880733))))









