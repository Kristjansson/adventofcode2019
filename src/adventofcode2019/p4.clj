(ns adventofcode2019.p4)


(comment ;;part 1
  (prn (time
         (count (for [i1 (range 4 9)
                      i2 (range i1 10)
                      i3 (range i2 10)
                      i4 (range i3 10)
                      i5 (range i4 10)
                      i6 (range i5 10)
                      :let [iv [i1 i2 i3 i4 i5 i6]]
                      :when (and (= -1 (compare [4 0 2 3 2 8] iv))
                                 (= -1 (compare iv [8 6 4 2 4 7])))
                      :when (or (= i1 i2)
                                (= i2 i3)
                                (= i3 i4)
                                (= i4 i5)
                                (= i5 i6))]
                  1)))))


(comment ;; part 2
  (prn (time
         (count (for [i1 (range 4 9)
                      i2 (range i1 10)
                      i3 (range i2 10)
                      i4 (range i3 10)
                      i5 (range i4 10)
                      i6 (range i5 10)
                      :let [iv [i1 i2 i3 i4 i5 i6]]
                      :when (and (= -1 (compare [4 0 2 3 2 8] iv))
                                 (= -1 (compare iv [8 6 4 2 4 7])))
                      :when (or (and (= i1 i2) (not= i2 i3))
                                (and (not= i1 i2) (= i2 i3) (not= i3 i4))
                                (and (not= i2 i3) (= i3 i4) (not= i4 i5))
                                (and (not= i3 i4) (= i4 i5) (not= i5 i6))
                                (and (not= i4 i5) (= i5 i6)))]
                  1)))))
