(ns dice-racing.development
  (:use clojure.core dice-racing.drivers dice-racing.data dice-racing.util dice-racing.trouble))
;(use 'clojure.core)

; ----------------------------------
;  data

(def *Game* [])
(def *Race* (ref {:current-lap 0 :lap-results {}
                  :lap-count {} :lap-status {}
                  :caution {} :roll-archive {}
                  :driver-status {} :track nil} ))

(defn restart-race []
  (dosync (ref-set *Race* {:current-lap 0 :lap-results {} :caution false :roll-archive {} :driver-status {} :track nil})))

(restart-race)


; ----------------------------------
;  functions



(defn run-normal
  "Lookup every index in the vector of indices, rs, in the vector tbl.
  Return the sum of the values, tbl[rs]."
  [& {:keys [tbl rolls]}]
  (reduce + (map tbl rolls)))

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
                                :tbl-speed   (prob-tbl (speed-tbl d))
                                :tbl-trbl    (prob-tbl (get tbl-race-trouble trbl-rating))}})]
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


(defn qualify-trouble []
  (nth tbl-qual-trouble (first (roll-dice 1 10))))


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



(defn update-game [r]
  (def *Game* (into *Game* [r])))


(defn run-lap
  [d R]
  (letfn [(last-speed [R d]
                         (first (for [[driver speed] (:lap-results R) :when (= driver d)] speed)))

          (process-trouble []
                           (let [trbl-tbl (:tbl-trbl d)
                                 [r] (roll-dice 1 100)
                                 trbl-number (get trbl-tbl r)
                                 race-order (into [] (for [[a _] (:lap-results @*Race*)] a))
                                 rest-of-pack  (reverse (take-while
                                                        (fn [driver]
                                                            (not (= (:d# d) driver)))
                                                          (reverse race-order)))]
                             (cond (< trbl-number 100) (do
                                                         (update-race *Race* :caution true)
                                                         (car-hits-wall-hard  d rest-of-pack)
                                                         ))))


          (get-drivers-fn [d]
            (second (get (:lap-status R) (:d# d))))

          (updated-speed [rs]
                         (+ (last-speed R (:d# d))
                            ((get-drivers-fn d) :rolls rs)))]


    (let [rolls (roll-dice (:rolls (:track R)) 100)
          tbl (:tbl-speed d)]
      (if (some nil? rolls)
        (process-trouble)
        (do
          [(:d# d) (updated-speed rolls)])))))

(defn get-drivers-fn [d# R]
  (second (get (:lap-status R) d#)))

@*Race*
(run-lap  (get roster 60) @*Race*)



(let [f (get-drivers-fn 60 @*Race*)
      t (:tbl-speed (get roster 60))]
  (f :tbl t :rolls [50 50]))
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

  (let [dat (into {}
                  (for [{k :d# tbl :tbl-speed} (vals roster)] [k [:normal (fn [& {:keys [rolls]}]
                                                       (run-normal :tbl tbl :rolls rolls))]]))]
  (update-race *Race* :lap-status dat))

  (def qual-results (qualify-all roster (:type (:track @*Race*))))
  (def race-order (process-qualify-results qual-results (:track @*Race*)))
  (def initial-speed-vals (build-starting-grid race-order))
  (update-race *Race* :lap-results initial-speed-vals)
)


Create a Function to update the lap status functions for the Race -- test it here
and then use it in the Trouble file


; run a lap
(do
  (update-race *Race* :lap-results
             (sort-by #(second %) >
                      (map (fn [[driver currSpeed]]
                             (run-lap (get roster driver) @*Race*))
                           (:lap-results @*Race*))))
  (update-race *Race* :current-lap (inc (:current-lap @*Race*))))

@*Race*
