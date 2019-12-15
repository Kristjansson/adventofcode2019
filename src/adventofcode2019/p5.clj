(ns adventofcode2019.p5
  (:require [clojure.string :as str]
            [criterium.core :as criterium]
            [adventofcode2019.core :as core]))


(comment
  (criterium/quick-bench (= 1 (/ (mod 10100 1000) 100)))
  (criterium/quick-bench (= \1 (.charAt ^String (str 10100) 2))))


(defn param-modes
  [param-mode opcode]
  (let [param-count (case opcode
                      (99) 0
                      (3 4) 1
                      (1 2) 3)]

    (->> (concat (reverse (str param-mode)) (repeat \0))
         (take param-count)
         (map #(- (int %) 48)))))


(defprotocol Parameter
  (read [this])
  (write [this value]))

(defrecord PositionalParameter
    [memory addr]

  Parameter
  (read [this] (get memory addr))
  (write [_ value] (assoc memory addr value)))

(defrecord ImmediateParameter
    [memory value]

  Parameter
  (read [this] value)
  (write [_ _] (throw (ex-info "Can't write to an immediate parameter" {}))))


(defn parameters
  [memory stack-ptr param-modes]
  (map (fn [mode offset]
         ((case mode
             0 ->PositionalParameter
             1 ->ImmediateParameter)
          memory
          (get memory (+ stack-ptr offset 1))))
       param-modes (range)))


(defn step
  [[memory stack-ptr]]
  (let [instruction (get memory stack-ptr)
        opcode (mod instruction 100)
        p-modes (param-modes (int (/ instruction 100)) opcode)
        params (parameters memory stack-ptr p-modes)]
    [(apply
       (case opcode ;; update memory
         ;; add
         1 (fn [in1 in2 out] (write out (+ (read in1) (read in2))))
         ;; multiply
         2 (fn [in1 in2 out] (write out (* (read in1) (read in2))))
         ;; input
         3 (fn [in] (write in (read-string (read-line))))
         ;; output
         4 (fn [in] (do
                      (prn (read in))
                      memory))
         ;; unknown - dump memory
         (fn [& _] memory))
       params)

     (case opcode ;; update stack-ptr
       (1 2) (+ 4 stack-ptr)
       (3 4) (+ 2 stack-ptr)
       (do
         (println "saw exit code:" opcode)
         -1))]))


(defn run-program
  [program]
  (loop [memory program
         op-ptr 0]
    (if (<= 0 op-ptr)
      (let [[memory' op-ptr'] (step [memory op-ptr])]
        (recur memory' op-ptr'))
      memory)))


(comment ;; Part 1
  (let [program (core/read-int-array "resources/05/1.txt")]
    (run-program program))


  )



;; (step (step (step [[03 1 004 1 99] 0])))



