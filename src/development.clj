(defn roll-dice
  ;roll a 's' sided dice 'n' times
  [n s]
  (if (= n 0)
    ()
    (conj (roll-dice (dec n) s) (rand-int s))))


(defn lookup-qual-rating
  [d]
  (get d :qual-rating))

(defn create-prob-tbl
  ; given a distribution d, create a lookup table with length 100
  [d]

  (reduce (fn [p v] (into p (take (get v 1)
                                  (iterate int (get v 0)))))
            [] d))

(defn get-qual-speed
  [d r]
  (let [prob-tbl (create-prob-tbl ((:qual-rating d) tbl-qual-speed-rating))]
    (get prob-tbl r)))

(def tbl-qual-speed-rating {:a qual-speed-rating-a
                            :b qual-speed-rating-b
                            :c qual-speed-rating-c
                            :d qual-speed-rating-d
                            :e qual-speed-rating-e})

(def qual-speed-rating-a [[12 10]
                        [13 10]
                        [14 10]
                        [15 20]
                        [16 16]
                        [17 9]
                        [18 24]
                        [nil 1]])

(def qual-speed-rating-b [[12 15]
                        [13 10]
                        [14 10]
                        [15 20]
                        [16 16]
                        [17 9]
                        [18 19]
                        [nil 1]])

(def qual-speed-rating-c [[12 20]
                        [13 10]
                        [14 10]
                        [15 20]
                        [16 16]
                        [17 9]
                        [18 14]
                        [nil 1]])

(def qual-speed-rating-d [[12 25]
                        [13 10]
                        [14 10]
                        [15 20]
                        [16 16]
                        [17 9]
                        [18 9]
                        [nil 1]])

(def qual-speed-rating-e [[12 30]
                        [13 10]
                        [14 10]
                        [15 20]
                        [16 16]
                        [17 9]
                        [18 4]
                        [nil 1]])


(defn test-prob-table
  ; test that prob table sums to 100
  [t]
  (= 100
     (reduce (fn [m v]
          (+ m (get v 1))) 0 t)))




;(test-prob-table (:e tbl-qual-speed-rating))

(get-qual-speed-rating driver48 (first (roll-dice 1 100)) )
(get-qual-speed-rating driver48 78)

(defn run-qualify-lap
  [d t]
  (map (fn [v]
       (get-qual-speed d v))
     (roll-dice ((get t :type) track-qualify-rolls) 100)))

(map (fn [v]
       (get-qual-speed driver48 v))
     (roll-dice 3 100))


(run-qualify-lap driver48 atlanta)

;data for testing
(def driver48 {:qual-rating :a})

;data for track
(def track-qualify-rolls {:short 3 :speed 4 :super 5 :road 5})

;track data
(def atlanta {:type :speed :name "Atlanta Motor Speedway" :rolls 1 :pit-window 8 :pit-stops 2 :total-laps 20 })



(:qual-rating driver48)