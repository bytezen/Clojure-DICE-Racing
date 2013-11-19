(ns dice-racing.development
  (:use clojure.core dice-racing.drivers dice-racing.data dice-racing.util))
;(use 'clojure.core)

; ----------------------------------
;  data


(def Race {:current-lap [0] :lap-results [] :caution {} :roll-archive []} )

(def race-roster [driver48 driver24 driver07 driver17 driver5 driver20 driver2 driver31])

; ----------------------------------
;  functions


(defn get-race-roster [ds {tt :type}]
  (letfn [(rating [d t]
                  ((t d)))
          (prob-tbl [d t]
                    (cond
                     (= t :short) (:tbl-short-track d)
                     (= t :speed) (:tbl-speed-track d)
                     (= t :super) (:tbl-super-track d)
                     (= t :road ) (:tbl-road-track d)
                     :else [0 0 0]))]
    (map #(let [{:keys [driver-name number qual-rating trbl-rating]} %]
             {:driver-name driver-name
              :number      number
              :qual-rating qual-rating
              :trbl-rating trbl-rating
              :tbl-speed   (prob-tbl % tt)}) ds)))


(defn get-qual-speed
  [{qual-rating :qual-rating} roll]
  (let [ prob (prob-tbl (get tbl-qual-speed-rating qual-rating))]
  (get prob roll)))

  [{qual-rating :qual-rating} r]
  (let [roll (if (vector? r) (first r) r)
        prob (prob-tbl (get tbl-qual-speed-rating qual-rating))]
  (get prob roll)))



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
                           (sort-by #(second %) < (into [] results)))
          (calculate-dnq [results]
                         (for [rec results :when (some nil? (second rec))]
                           [(first rec) (qualify-trouble)]))]
       (into  (calculate-dnq qs) (into [] (sort-qualifying (calculate-qualify-mph qs t))))))



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
          (conj qualify-speed-mph dnqs)))


"(defn filter-no-trouble
  [record]
  (not-any? nil? (second record)))


(defn filter-had-trouble
  [record]
  (some nil? (second record)))

(defn sort-hash
  [h]
  (sort-by (fn [v] (second v))  h))
"

"(defn get-qualify-speed
  [track results]
  (let [no-trouble (filter #(filter-no-trouble %) results)
        qual-speed (map (fn [v] {(first v) (qual-speed-rating-to-mph track (reduce + (second v)))})
                                       no-trouble)
        trouble    (filter #(filter-had-trouble %) results)]
    ;(conj (for [rec trouble]  {(first rec) (qualify-trouble)})
        (first qual-speed)))"



(defn qualify-trouble []
  (nth tbl-qual-trouble (first (roll-dice 1 10))))


(defn build-starting-grid [drivers]
  "given an ordered sequence of drivers returns the starting penalty points for each driver;
  The (pos-driver ...) is a neat-o trick I learned from here (see comments)
  http://stackoverflow.com/questions/4830900/how-do-i-find-the-index-of-an-item-in-a-vector "
  (letfn [(get-penalty [pos mxPenalty mxRows]
                       (Math/floor (lmap 1 mxRows 0.0 mxPenalty (Math/round (* pos 0.5)))))]

    (let [cnt (count drivers)
          rows (Math/round (* cnt 0.5))
          max-penalty (cond
                       (< rows 5) 3
                       (< rows 8) 4
                       :else      6)
          pos-driver (map vector (iterate inc 1)
                          drivers)]

          (map (fn [[x [y _]]]
                 [ y (inv (get-penalty x max-penalty rows))])
               pos-driver))))



(positions [1 2 3])

(defn clamp [a b val]
  (min (max a val) b))


(defn lmap [a b x y val]
    (let [cval (clamp a b val)
          alpha (/ (- cval a) (- b a))
          beta (- 1 alpha)]
      (+ (* beta x) (* alpha y ))))

(defn inv [a] (cond (or (= 0.0 a) (= 0 a)) a
                    :else (* -1 a)))

(defn update-race [race k v]
  "get a new race with the new value,v, conjoined to the key"
  (assoc race k (conj (k race) v)))

; ----------------------------------
;    RUNNING - TESTING
; ----------------------------------

(def qual-results (qualify-all race-roster atlanta))
(def race-order (process-qualify-results qual-results atlanta))
(def initial-speed-vals (build-starting-grid race-order))


(def Race-Qual (update-race Race :roll-archive qual-results))
(update-race Race-Qual :lap-results initial-speed-vals)
