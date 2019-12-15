(ns adventofcode2019.p5
  (:require [clojure.string :as str]
            [criterium.core :as criterium]
            [adventofcode2019.core :as core]))


(comment
  (criterium/quick-bench (= 1 (/ (mod 10100 1000) 100)))
  (criterium/quick-bench (= \1 (.charAt ^String (str 10100) 2))))


(defn read
  [memory addr]
  (case addr
    -1 (read-string (read-line))
    (get memory addr)))


(defn write
  [memory addr value]
  (case addr
    -1 (do (prn value) memory)
    (assoc memory addr value)))


(defn step
  [[memory stack-ptr]]
  (let [read' (partial read memory)
        write' (partial write memory)
        opcode (get memory stack-ptr)]
    [(case opcode ;; update memory
       ;; add
       1 (write' (read' (+ 3 stack-ptr))
                 (+ (read' (read' (+ 1 stack-ptr)))
                    (read' (read' (+ 2 stack-ptr)))))
       101 (write' (read' (+ 3 stack-ptr))
                   (+ (read' (+ 1 stack-ptr))
                      (read' (read' (+ 2 stack-ptr)))))
       1001 (write' (read' (+ 3 stack-ptr))
                    (+ (read' (read' (+ 1 stack-ptr)))
                       (read' (+ 2 stack-ptr))))
       1101 (write' (read' (+ 3 stack-ptr))
                    (+ (read' (+ 1 stack-ptr))
                       (read' (+ 2 stack-ptr))))

       ;; multiply
       2 (write' (read' (+ 3 stack-ptr))
                 (* (read' (read' (+ 1 stack-ptr)))
                    (read' (read' (+ 2 stack-ptr)))))
       102 (write' (read' (+ 3 stack-ptr))
                   (* (read' (+ 1 stack-ptr))
                      (read' (read' (+ 2 stack-ptr)))))
       1002 (write' (read' (+ 3 stack-ptr))
                    (* (read' (read' (+ 1 stack-ptr)))
                       (read' (+ 2 stack-ptr))))
       1102 (write' (read' (+ 3 stack-ptr))
                    (* (read' (+ 1 stack-ptr))
                       (read' (+ 2 stack-ptr))))

       ;; input
       3 (write' (read' (+ 1 stack-ptr))
                 (read' -1))

       ;; output
       4 (write' -1 (read' (read' (+ 1 stack-ptr))))
       104 (write' -1 (read' (+ 1 stack-ptr)))

       ;; unknown - dump memory
       memory)

     (case opcode ;; update stack-ptr
       (1 101 1001 1101 2 102 1002 1102) (+ 4 stack-ptr)
       (3 4 104) (+ 2 stack-ptr)

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



