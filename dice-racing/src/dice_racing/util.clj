(ns dice-racing.util
  (:use clojure.core))



(defn roll-dice
  ;roll a 's' sided dice 'n' times
  [n s]
  (if (= n 0)
    []
    (conj (roll-dice (dec n) s) (rand-int s))))
