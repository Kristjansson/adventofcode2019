(ns adventofcode2019.core
  (:require
    [clojure.string :as str]))


(defn read-lines
  [file-name]
  (-> (slurp file-name)
      (str/split-lines)))


(defn read-int-array
  [file-name]
  (->> (str/split (str/trim (slurp file-name)) #"\,")
       (mapv #(Integer/parseInt %))))

