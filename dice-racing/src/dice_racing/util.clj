(ns dice-racing.util
  (:use clojure.core))



(defn roll-dice
  ;roll a 's' sided dice 'n' times
  [n s]
  (if (= n 0)
    []
    (conj (roll-dice (dec n) s) (rand-int s))))



(defn clamp [a b val]
  (min (max a val) b))


(defn lmap [a b x y val]
    (let [cval (clamp a b val)
          alpha (/ (- cval a) (- b a))
          beta (- 1 alpha)]
      (+ (* beta x) (* alpha y ))))

(defn inv [a] (cond (or (= 0.0 a) (= 0 a)) a
                    :else (* -1 a)))



(defmacro update-race
  "update race, r, with the value, v, for key, k"
  [r k v]
  (list 'dosync (list 'alter r 'assoc  k v)))
