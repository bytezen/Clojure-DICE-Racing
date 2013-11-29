(ns dice-racing.trouble
  (:use dice-racing.util))
; ------------------
; Race Trouble Fn

(defn under-caution [s]

  s)

(defn pit [s]
  s)

(defn car-out-of-race [s]
  [:dnf s])

(defn spin-out
  []
  (under-caution "Car loses control and spins out. Caution flag is up."))

(defn debris
  []
  (under-caution "Caution is out for debris on the track"))

(defn flat-tire
  []
  (pit "Your tire is going flat. You have to pit now!"))

(defn dnf-blown-engine [_]
  (car-out-of-race "Blown Engine. Car out of race"))

(defn dnf-blown-clutch [_]
  (car-out-of-race "Blown Clutch. Car out of race"))

(defn dnf-brake-failure [_]
  (car-out-of-race "Brake Failure. Car out of race"))

(defn dnf-blown-engine-caution? []
  "Car is out of race with blown engine, but check for caution"
  (cond (< 3 (first (roll-dice 1 10)))
        (under-caution (str (:d# d)
                            " blew the engine and is out of the race. We are under caution "))
        :else dnf-blown-engine))

(defn car-trouble [n s]
  [n (str "Car is " s " Take only " n " speed")])

(defn car-loose-7 []
  (car-trouble 7 "loose"))

(defn car-loose-8 []
  (car-trouble 8 "loose"))

(defn car-loose-9 []
  (car-trouble 9 "loose"))

(defn car-tight-7 []
  (car-trouble 7 "tight"))

(defn car-tight-8 []
  (car-trouble 8 "tight"))

(defn car-tight-9 []
  (car-trouble 9 "tight"))

(defn setup-trouble-1! []
  (fn [speed] (dec speed)))

(defn setup-trouble-2! []
  (fn [speed] (- 2 speed)))

(defn scrape-wall! []
  (fn [speed] (dec speed)))

(defn car-hits-wall []
  (pit "Car hits the wall. You must pit now...")
  (cond (< 5 (first (roll-dice 1 10))) (under-caution "... we are under caution")
        :else "...no caution keep racin'"))

(defn blown-tire []
  (pit "You blew a tire. You must pit now...")
  (cond (< 5 (first (roll-dice 1 10)) )
        (under-caution "...there's debris on the track. We are under caution.")
        :else "...no caution keep racin"))

