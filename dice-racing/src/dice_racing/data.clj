(ns dice-racing.data
  (:use clojure.core dice-racing.util))



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

(def tbl-qual-speed-rating {:a qual-speed-rating-a
                            :b qual-speed-rating-b
                            :c qual-speed-rating-c
                            :d qual-speed-rating-d
                            :e qual-speed-rating-e})


(def tbl-qual-trouble ["Blown Engine"
      "No brakes"
      "Broken Clutch"
      "Car overheating"
      "Crash, Hit the wall hard"
      "Spin out."
      "Failed pre-race inspection"
      "Blown tire"
      "Loose lug nut falls off car"
      "Driving below the line"])

; ---------------------------
; RACE TROUBLE ROLL Tables

(def race-trbl-roll-table-a [[ 1  5]
                         [ 2  5]
                         [ 3  5]
                         [ 4  5]
                         [ 5  5]
                         [10 10]
                         [11 10]
                         [12  5]
                         [13 10]
                         [14 10]
                         [15  5]
                         [16  5]
                         [17  5]
                         [18  5]
                         [19  5]
                         [20  5]])

(def race-trbl-roll-table-b [[ 1  5]
                         [ 2  5]
                         [ 3  5]
                         [ 4  5]
                         [ 5  5]
                         [ 9  5]
                         [10 10]
                         [11 10]
                         [12  5]
                         [13 10]
                         [14  5]
                         [15  5]
                         [16  5]
                         [17  5]
                         [18  5]
                         [19  5]
                         [20  5]])

(def race-trbl-roll-table-c [[ 1  5]
                         [ 2  5]
                         [ 3  5]
                         [ 4  5]
                         [ 5  5]
                         [ 7  5]
                         [ 9  5]
                         [10 10]
                         [11  5]
                         [12  5]
                         [13 10]
                         [14  5]
                         [15  5]
                         [16  5]
                         [17  5]
                         [18  5]
                         [19  5]
                         [20  5]])

(def race-trbl-roll-table-d [[ 1  5]
                         [ 2  5]
                         [ 3  5]
                         [ 4  5]
                         [ 5  5]
                         [ 7  5]
                         [ 8  5]
                         [ 9  5]
                         [10 10]
                         [11  5]
                         [12  5]
                         [13  5]
                         [14  5]
                         [15  5]
                         [16  5]
                         [17  5]
                         [18  5]
                         [19  5]
                         [20  5]])

(def race-trbl-roll-table-e [[ 1  5]
                         [ 2  5]
                         [ 3  5]
                         [ 4  5]
                         [ 5  5]
                         [ 6  5]
                         [ 7  5]
                         [ 8  5]
                         [ 9  5]
                         [10  5]
                         [11  5]
                         [12  5]
                         [13  5]
                         [14  5]
                         [15  5]
                         [16  5]
                         [17  5]
                         [18  5]
                         [19  5]
                         [20  5]])

(def tbl-race-trouble {:a race-trbl-roll-table-a
                            :b race-trbl-roll-table-b
                            :c race-trbl-roll-table-c
                            :d race-trbl-roll-table-d
                            :e race-trbl-roll-table-e})




; --------------------



;data for track
(def track-qualify-rolls {:short 3 :speed 4 :super 5 :road 5})

;track data
(def track {:atlanta {:type :speed :name "Atlanta Motor Speedway" :rolls 1 :pit-window 8 :pit-stops 2 :total-laps 20 }})

(defn prob-tbl
  [distribution]
  (let [d (flatten distribution)
        total-prob (reduce + (flatten (partition 1 2 (rest d))))]
    (cond (= total-prob 100)
          (into [] (flatten (for [[a b] (partition 2 d)]
                       (repeat b a))))
          :else (throw (Exception. (format "\n***\nProbability does not sum to 100; SUM = %s\n***" total-prob ))))))
