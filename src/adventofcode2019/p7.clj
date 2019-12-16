(ns adventofcode2019.p7
  (:require
    [adventofcode2019.core :as core]
    [adventofcode2019.intcode :as intcode]))


(defn run-program
  [program & input]
  (loop [memory program
         op-ptr 0
         input input
         output []]
    (if (<= 0 op-ptr)
      (let [[memory' op-ptr' input' output'] (intcode/step [memory op-ptr input output])]
        (recur memory' op-ptr' input' output'))
      output)))


(defn run-amplifiers
  [program modes]
  (let [run-amp (comp first (partial run-program program))]
    (->> (run-amp (nth modes 0) 0)
         (run-amp (nth modes 1))
         (run-amp (nth modes 2))
         (run-amp (nth modes 3))
         (run-amp (nth modes 4)))))

(assert (= 43210
           (run-amplifiers
             [3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0]
             [4 3 2 1 0])))


(assert (= 54321
           (run-amplifiers
             [3,23,3,24,1002,24,10,24,1002,23,-1,23,101,5,23,23,1,24,23,23,4,23,99,0,0]
             [0,1,2,3,4])))

(assert (= 65210
           (run-amplifiers
             [3,31,3,32,1002,32,10,32,1001,31,-2,31,1007,31,0,33,1002,33,7,33,1,33,31,31,1,32,31,31,4,31,99,0,0,0]
             [1,0,4,3,2])))

(def program (core/read-int-array "resources/07/1.txt"))

(->> (for [i1 (range 5)
           i2 (range 5)
           i3 (range 5)
           i4 (range 5)
           i5 (range 5)
           :let [modes [i1 i2 i3 i4 i5]]
           :when (= 5 (count (set modes)))]
       (run-amplifiers program modes))
     (apply max))

;; scratch

(defn run-programs
  [machine-state]
  (prn "Starting")
  (loop [states machine-state
         itr 0
         outputs {}]
    (if (seq states)
      (let [k (rand-nth (keys states))]
        (let [op-ptr (get (get states k) 1)]
          (if (<= 0 op-ptr)
            (recur (update states k intcode/step)
                   (inc itr)
                   outputs)
            ;; thread done
            (recur (dissoc states k)
                   (inc itr)
                   (assoc outputs k (last (get states k)))))))
      outputs)))




(let [p (core/read-int-array "resources/05/1.txt")]
  ;; (run-program p [1])
  (prn (run-programs
         {:a [p 0 [1] []]
          :b [p 0 [5] []]})))


(defn run-thread
  [program])


(comment ;; Part 1
  (let [program (core/read-int-array "resources/05/1.txt")]
    (run-program program [5]))

  (run-program [3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0])

  (run-program [3,9,7,9,10,9,4,9,99,-1,8])
  (run-program [3,3,1108,-1,8,3,4,3,99])
  (run-program [3,3,1107,-1,8,3,4,3,99])
  (run-program [3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9])
  (run-program [3,3,1105,-1,9,1101,0,0,12,4,12,99,1])
  (run-program [3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,
                1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,
                999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99]))

;; other idea - make memory a 2d address (program, addr)
