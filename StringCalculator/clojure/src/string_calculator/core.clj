(ns string-calculator.core
  (:require [defun.core :as defun]
            [clojure.string :as str]))

;; TODO: Find out why does my linter show errors on add' and numbers' ? 
(defun/defun add'
  ([""] 0)
  (["1"] 1)
  ([numbers] 3))

(defn- calculate [numbers delimiters]
  (let [ints (map #(Integer/parseInt %) (str/split numbers (re-pattern delimiters)))
        negatives (filterv #(< % 0) ints)]
    (cond
      (empty? negatives)
      (reduce + 0 ints)

      :else (throw (Exception. (str "negatives not allowed : " negatives)))
      )))

(defn add [numbers]
  (cond
    (= numbers "") 0

    (str/starts-with? numbers "//")
    (let [[delimiters new-numbers] (str/split (subs numbers 2) #"\n")]
      (calculate new-numbers delimiters))

    :else (calculate numbers ",|\n")))
