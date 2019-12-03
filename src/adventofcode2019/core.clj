(ns adventofcode2019.core
  (:require
    [clojure.string :as str]))


(defn read-lines
  [file-name]
  (-> (slurp file-name)
      (str/split-lines)))
