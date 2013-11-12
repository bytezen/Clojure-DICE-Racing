(ns dice-racing.data)
(use 'clojure.core)


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


;data for track
(def track-qualify-rolls {:short 3 :speed 4 :super 5 :road 5})

;track data
(def atlanta {:type :speed :name "Atlanta Motor Speedway" :rolls 1 :pit-window 8 :pit-stops 2 :total-laps 20 })
