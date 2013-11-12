(ns dice-racing.core
  (:gen-class)
  (:use dice-racing.development))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  ;; work around dangerous default behaviour in Clojure
  (alter-var-root #'*read-eval* (constantly false))
  (println "Hello, World!"))


(get-qual-speed driver48 (first (roll-dice 1 100)) )
