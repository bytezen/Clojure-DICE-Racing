   (ns scripts.driver-data-parsing
     (:use clojure.java.io))

   (defn test [] (prn "hello"))

   (with-open [rdr (reader "resources/dat/driver_all.raw")]
     (doseq [line (line-seq rdr)]
       (println line)))
