(ns adventofcode2019.intcode)


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
  [[memory stack-ptr input output]]
  (let [instruction (get memory stack-ptr)
        opcode (mod instruction 100)
        p-modes (param-modes (int (/ instruction 100)) opcode)
        params (parameters memory stack-ptr p-modes)]
    [(apply ;; update memory
       (case opcode
         ;; add
         1 (fn [in1 in2 out] (write out (+ (read in1) (read in2))))
         ;; multiply
         2 (fn [in1 in2 out] (write out (* (read in1) (read in2))))
         ;; input
         3 (fn [in] (if (seq input)
                      (write in (first input))
                      memory))
         ;; output
         4 (fn [in] memory)
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
         3 (fn [& _]
             (if (seq input)
               (+ 2 stack-ptr)
               stack-ptr))
         4 (fn [& _] (+ 2 stack-ptr))
         5 (fn [in1 in2] (if-not (zero? (read in1))
                           (read in2)
                           (+ 3 stack-ptr)))
         6 (fn [in1 in2] (if (zero? (read in1))
                           (read in2)
                           (+ 3 stack-ptr)))
         (fn [& _]
           ;; (println "saw exit code:" opcode)
           -1))
       params)

     (case opcode ;; update input
       (1 2 4 5 6 7 8) input
       3 (rest input)
       input)


     (case opcode ;; update output
       (1 2 3 5 6 7 8) output
       4 (conj output (read (first params)))
       output)]))


(defn run-program
  [program input]
  (loop [memory program
         op-ptr 0
         input input
         output []]
    (if (<= 0 op-ptr)
      (let [[memory' op-ptr' input' output'] (step [memory op-ptr input output])]
        (recur memory' op-ptr' input' output'))
      output)))



(defn run-thread
  [program])
