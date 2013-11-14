(ns dice-racing.development
  (:use clojure.core dice-racing.drivers dice-racing.data))
;(use 'clojure.core)

; ----------------------------------
;  data


(def race-roster [driver48 driver24 driver07 driver17 driver5 driver20 driver2 driver31])

; ----------------------------------
;  functions

(defn roll-dice
  ;roll a 's' sided dice 'n' times
  [n s]
  (if (= n 0)
    []
    (conj (roll-dice (dec n) s) (rand-int s))))



(defn create-prob-tbl
  ; given a distribution d, create a lookup table with length 100
  [d]
  (reduce (fn [p v]
            (into p (repeat (get v 1) (get v 0))))
            [] d))


(defn get-qual-speed
  [{qual-rating :qual-rating} roll]
  (let [prob-tbl (create-prob-tbl (get tbl-qual-speed-rating qual-rating))]
    (get prob-tbl roll)))


(defn test-prob-table
  ; test that prob table sums to 100
  [t]
  (= 100
     (reduce (fn [m v]
          (+ m (get v 1))) 0 t)))




(defn qualify-all
  [ds {track-type :type}]
  (letfn [(qual [driver t]
                (into [] (map (fn [v]
                                (get-qual-speed driver v))
                              (roll-dice (get track-qualify-rolls t) 100))) )]

    (apply hash-map (interleave (map :number ds)
                                (map (fn [d] (qual d track-type))
                                     ds)))))


(defn qual-speed-rating-to-mph
  "convert from speed rating total to miles per hour.
  t: track
  r: total quality speed roll"
  [t roll-total]
  (let [trk-type (:type t)
        spd (cond (= trk-type :short)  (lmap 36 54 126.5 128.3 roll-total)
                  (= trk-type :super)  (lmap 60 89 189.1 192.0 roll-total)
                  (= trk-type :road)   (lmap 60 89 121.6 124.5 roll-total)
                  (= trk-type :speed)  (lmap 48 72 187.5 189.9 roll-total)
                  :else 0)]
    (read-string (format "%.3f"
                         (+ (read-string (format "%.1f" spd))
                            (/ (first (roll-dice 1 100))
                                      1000.0))))))



(defn process-qualify-results
  "qualify a race-roster for a track.
    return: a hash map of driver number and qualifying lap times sorted"
  [qs t]
  (letfn [
          (calculate-qualify-mph [results track]
                                 (for [rec results :when (not-any? nil? (second rec))]
                                   [(first rec) (qual-speed-rating-to-mph track (reduce + (second rec)))]))
          (sort-qualifying [results]
                           (sort-by #(second %) > (into [] results)))
          (calculate-dnq [results]
                         (for [rec results :when (some nil? (second rec))]
                           [(first rec) (qualify-trouble)]))]

       (conj  (calculate-dnq qs) (sort-qualifying (calculate-qualify-mph qs t)))))



(defn total-qual-speed
  [track qual-results]
  (letfn [(get-qualified []
           (for [r (keys qual-results)
                 :when (not-any? nil? (get qual-results r))] r))

          (get-did-not-qualify []
            (for [r (keys qual-results) :when (some nil? (get qual-results r))] r ))]

          (def qs (get-qualified))
          (def dnqs (get-did-not-qualify))
          (def qualify-speed-mph (map #(qual-speed-rating-to-mph track %)
                                      (map #(reduce + %)
                                           (map #(get qual-results %)
                                                qs))))

;          (conj (qual-speed-rating-to-mph track (map #(reduce + %)
;                 (map #(get qual-results %)
;                       qs)))
;                dnqs)))
          (conj qualify-speed-mph dnqs)))


(defn filter-no-trouble
  [record]
  (not-any? nil? (second record)))


(defn filter-had-trouble
  [record]
  (some nil? (second record)))

(defn sort-hash
  [h]
  (sort-by (fn [v] (second v))  h))


(defn get-qualify-speed
  [track results]
  (let [no-trouble (filter #(filter-no-trouble %) results)
        qual-speed (map (fn [v] {(first v) (qual-speed-rating-to-mph track (reduce + (second v)))})
                                       no-trouble)
        trouble    (filter #(filter-had-trouble %) results)]
    ;(conj (for [rec trouble]  {(first rec) (qualify-trouble)})
        (first qual-speed)))


(defn qualify-trouble []
  (nth tbl-qual-trouble (first (roll-dice 1 10))))


(defn clamp [a b val]
  (min (max a val) b))


(defn lmap [a b x y val]
    (let [cval (clamp a b val)
          alpha (/ (- cval a) (- b a))
          beta (- 1 alpha)]
      (+ (* beta x) (* alpha y ))))

; ----------------------------------
;    RUNNING - TESTING
; ----------------------------------

(def qual-results (qualify-all race-roster atlanta))
(process-qualify-results qual-results atlanta)

