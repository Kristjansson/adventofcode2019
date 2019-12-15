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
                      (5 6) 2
                      (1 2 7 8) 3)]

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
         ;; jumps
         (5 6) (fn [& _] memory)
         ;; less than
         7 (fn [in1 in2 out] (write out (if (< (read in1) (read in2)) 1 0)))
         ;; equal to
         8 (fn [in1 in2 out] (write out (if (= (read in1) (read in2)) 1 0)))
         ;; unknown - dump memory
         (fn [& _] memory))
       params)

     (apply
       (case opcode ;; update stack-ptr
         (1 2 7 8) (fn [& _] (+ 4 stack-ptr))
         (3 4) (fn [& _] (+ 2 stack-ptr))
         5 (fn [in1 in2] (if-not (zero? (read in1))
                           (read in2)
                           (+ 3 stack-ptr)))
         6 (fn [in1 in2] (if (zero? (read in1))
                           (read in2)
                           (+ 3 stack-ptr)))
         (fn [& _]
           (println "saw exit code:" opcode)
           -1))
       params)]))


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

  (run-program [3,9,8,9,10,9,4,9,99,-1,8])
  (run-program [3,9,7,9,10,9,4,9,99,-1,8])
  (run-program [3,3,1108,-1,8,3,4,3,99])
  (run-program [3,3,1107,-1,8,3,4,3,99])
  (run-program [3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9])
  (run-program [3,3,1105,-1,9,1101,0,0,12,4,12,99,1])
  (run-program [3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,
                1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,
                999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99]))
