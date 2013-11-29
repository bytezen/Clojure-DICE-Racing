(ns dice-racing.drivers)
(use 'clojure.core)

;-------------------------
;  DATA PROCESSING FN

(defn make-histogram
  "given a vector of counts and a range return a vector
  that can be used to construct a probability mass function table.
  The function expexts that count of the [counts] is 1 greater than bin count.
  The last [counts] is the count for the nil bin in the histogram"

  [[a b] counts]
  (into []
  (interleave (conj (into [] (range a (inc b)))
                             nil)
                       counts)))



;---------------------------
;  RECORDS



(def driver48 {:qual-rating :a
               :driver-name "Jimmie Johnson"
               :driver-rating :a
               :number 48
               :trbl-rating :a
               :short :a
               :super :a
               :speed :a
               :road :a
               :tbl-short-track []
               :tbl-super-track []
               :tbl-speed-track (make-histogram [14 18] [20 20 20 20 17 3] )
               :tbl-road-track []})

(def driver24 {:qual-rating :a
               :driver-name "Jeff Gordon"
               :driver-rating :a
               :number 24
               :trbl-rating :a
               :short :a
               :super :a
               :speed :a
               :road :a
               :tbl-short-track []
               :tbl-super-track []
               :tbl-speed-track  (make-histogram [14 18] [20 20 20 20 17 3])
               :tbl-road-track []})


(def driver33 {:qual-rating :b
               :driver-name "Clint Bowyer"
               :driver-rating :b
               :number 33
               :trbl-rating :a
               :short :a
               :super :b
               :speed :a
               :road :a
               :tbl-short-track []
               :tbl-super-track []
               :tbl-speed-track (make-histogram [14 18] [25 20 20 20 11 4])
               :tbl-road-track []})


(def driver17 {:qual-rating :b
               :driver-name "Matt Kenseth"
               :driver-rating :a
               :number 17
               :trbl-rating :b
               :speed :a
               :short :a
               :super :b
               :road :c
               :tbl-short-track []
               :tbl-super-track []
               :tbl-speed-track (make-histogram [14 18] [20 20 20 20 18 2])
               :tbl-road-track []})

(def driver18 {:driver-name "Kyle Busch"
               :driver-rating :a
               :number 18
               :qual-rating :a
               :trbl-rating :a
               :speed :a
               :short :a
               :super :c
               :road :a
               :tbl-short-track []
               :tbl-super-track []
               :tbl-speed-track (make-histogram [14 18] [20 20 20 20 17 3])
               :tbl-road-track []})

(def driver14 {:driver-name "Tony Stewart"
               :driver-rating :a
               :number 14
               :qual-rating :b
               :trbl-rating :b
               :speed :a
               :short :a
               :super :d
               :road :a
               :tbl-short-track []
               :tbl-super-track []
               :tbl-speed-track (make-histogram [14 18] [20 20 20 20 19 1])
               :tbl-road-track []})

(def driver22 {:driver-name "Kurt Busch"
               :driver-rating :a
               :number 22
               :qual-rating :a
               :trbl-rating :a
               :speed :b
               :short :b
               :super :a
               :road :b
               :tbl-short-track []
               :tbl-super-track []
               :tbl-speed-track (make-histogram [14 18]  [20 20 20 20 18 2])
               :tbl-road-track []})

(def driver31 {:driver-name "Jeff Burton"
               :driver-rating :b
               :number 31
               :qual-rating :b
               :trbl-rating :a
               :speed :b
               :short :b
               :super :c
               :road :c
               :tbl-short-track []
               :tbl-super-track []
               :tbl-speed-track (make-histogram [14 18] [25 20 20 20 12 3])
               :tbl-road-track []})

(def driver60 {:driver-name "Mike Skinner"
               :driver-rating :e
               :number 60
               :qual-rating :e
               :trbl-rating :e
               :speed :e
               :short :e
               :super :e
               :road :e
               :tbl-short-track []
               :tbl-super-track []
               :tbl-speed-track (make-histogram [14 18] [40 20 20 15 0 5])
               :tbl-road-track []})



(def driver38 {:driver-name "J.J. Yeley"
               :driver-rating :d
               :number 38
               :qual-rating :d
               :trbl-rating :e
               :speed :d
               :short :e
               :super :e
               :road :e
               :tbl-short-track []
               :tbl-super-track []
               :tbl-speed-track (make-histogram [14 18] [35 20 20 20 0 5])
               :tbl-road-track []})


