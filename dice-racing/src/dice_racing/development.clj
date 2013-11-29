(ns dice-racing.development
  (:use clojure.core dice-racing.drivers dice-racing.data dice-racing.util dice-racing.trouble))
;(use 'clojure.core)

; ----------------------------------
;  data

(def *Game* [])
(def *Race* (ref {:current-lap 0 :lap-results {} :caution {} :roll-archive {}  :driver-status {} :track nil} ))

(defn restart-race []
  (dosync (ref-set *Race* {:current-lap 0 :lap-results {} :caution {} :roll-archive {} :driver-status {} :track nil})))

(restart-race)
; ----------------------------------
;  functions

(defn get-race-roster
  [ds tt]
  (letfn [(rating [d t]
                  ((t d)))
          (speed-tbl [d]
                    (cond
                     (= tt :short) (:tbl-short-track d)
                     (= tt :speed) (:tbl-speed-track d)
                     (= tt :super) (:tbl-super-track d)
                     (= tt :road ) (:tbl-road-track d)
                     :else [0 0 0]))
          (make-record [{:keys [driver-name number qual-rating trbl-rating] :as d}]
                       {number {:driver-name driver-name
                                :d#      number
                                :qual-rating qual-rating
                                :trbl-rating trbl-rating
                                :tbl-speed   (prob-tbl (speed-tbl d))}}
                       )]
    (map #(make-record %) ds)))



(defn get-qual-speed
  [{qual-rating :qual-rating} roll]
  (let [ prob (prob-tbl (get tbl-qual-speed-rating qual-rating))]
  (get prob roll)))



(defn test-prob-table
  ; test that prob table sums to 100
  [t]
  (= 100
     (reduce (fn [m v]
          (+ m (get v 1))) 0 t)))




(defn qualify-all
  [ds track-type]
  (letfn [(qual [driver t]
                (into [] (map (fn [v]
                                (get-qual-speed driver v))
                              (roll-dice (get track-qualify-rolls t) 100))) )]

    "(apply hash-map (interleave (map :number ds)
                                (map (fn [d] (qual d track-type))
                                     ds)))))"
    (apply hash-map (interleave (keys ds)
                                (map (fn [d] (qual d track-type))
                                     (vals ds))))))



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



(defn clamp [a b val]
  (min (max a val) b))


(defn lmap [a b x y val]
    (let [cval (clamp a b val)
          alpha (/ (- cval a) (- b a))
          beta (- 1 alpha)]
      (+ (* beta x) (* alpha y ))))

(defn inv [a] (cond (or (= 0.0 a) (= 0 a)) a
                    :else (* -1 a)))

(defn update-race [ k v]
  "get a new race with the new value,v, conjoined to the key"
  (def *Race* (assoc Race k (conj (k *Race*) v))))

(defn update-game [r]
  (def *Game* (into *Game* [r])))


(defn run-lap
  [d R]
  (letfn [(last-speed [R d]
                         (for [[driver speed] (:lap-results R) :when (= driver d)] speed))

          (current-lap [prob rs]
             (if (not-any? nil? rs)
               (map prob rs)
               (fn [] "process trouble")))

          (updated-speed [rs]
                         (reduce + (into (current-lap (:tbl-speed d) rs)
                                         (last-speed R (:d# d)))))

          (get-tbl-speed [d]
                         )]

    (let [rolls (roll-dice (:rolls (:track R)) 100)
          tbl (:tbl-speed d)]

      [(:d# d) (updated-speed rolls)])))
  ;    (interleave (current-speed (:tbl-speed d) rolls) (last-speed R driverNum)))))




(keys @*Race*)
(:lap-results @*Race*)
(map #(run-lap % @*Race*) (:lap-results @*Race*))
(:lap-results @*Race*)

(keys roster)
(13.0 13.0 11.0 17.0 12.0 13.0 12.0 16.0 11.0 11.0)
*Race*
(get roster 17)


; ----------------------------------
;    RUNNING - TESTING
; ----------------------------------



; setup
(restart-race)
(update-race *Race* :track (:atlanta track))

;
; run the qualifying sequence
(do
  (def roster (into {} (get-race-roster [driver17 driver60 driver38 driver48 driver24
                                         driver33 driver18 driver14 driver22 driver31] (:type (:track @*Race*)) )))
  (def qual-results (qualify-all roster (:type (:track @*Race*))))
  (def race-order (process-qualify-results qual-results (:track @*Race*)))
  (def initial-speed-vals (build-starting-grid race-order))
)

; update race with qualifying results
(update-race *Race* :lap-results initial-speed-vals)

; run the race!!
(update-race *Race* :lap-results
             (sort-by #(second %) >
                      (map (fn [[driver currSpeed]]
                             (run-lap (get roster driver) @*Race*))
                           (:lap-results @*Race*))))






@*Race*
(macroexpand-1 '(update-race *Race* :current-lap 10))
Race
roster

(defmacro update-race
  "update race, r, with the value, v, for key, k"
  [r k v]
  (list 'dosync (list 'alter r 'assoc  k v)))

(do
  (def Race (restart-race))
  (def Race (assoc Race :track (:atlanta track)))
  (update-game Race)
  Race
  (def roster (get-race-roster [driver07] (:atlanta track)))
  (def qual-results (qualify-all roster (:atlanta track)))
  (def race-order (process-qualify-results qual-results (:atlanta track)))
  (def initial-speed-vals [[(:number driver07) 0]])
  (update-race :lap-results initial-speed-vals)
  (update-race :driver-status (map (fn [v]
                                     {(:number v) :normal})
                                   roster ))
  )



(def roster (into {} (get-race-roster [driver07 driver60 driver38] :speed)))

qual-results
Race
(def *Game* (into *Game* [Race]) )
*Game*
(def *Game* [])

(def qual-results (qualify-all race-roster atlanta))
qual-results
(def race-order (process-qualify-results qual-results atlanta))
race-order
(def initial-speed-vals (build-starting-grid race-order))
initial-speed-vals

(def Race-Qual (update-race Race :roll-archive qual-results))
(update-race Race-Qual :lap-results initial-speed-vals)

(def race-roster [driver48 driver24 driver07 driver17 driver5 driver20 driver2 driver31])

(type roster)
roster
(def d07 (first test-roster))


(run-lap (first roster) (:track Race))
