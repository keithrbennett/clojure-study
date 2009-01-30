; Simple Illustration of Clojure Concurrency


(ns concurrency-example)

(def reference-time (System/currentTimeMillis))


(defn create-log-entry [counter-val]
  (let 
    [
      msec-elapsed (- (System/currentTimeMillis) reference-time)
      sec-str (format "Counter value: %10d   Time: %12.3f sec    Thread: %s"
          (long counter-val)
          (double (/ msec-elapsed 1000))
          (. (Thread/currentThread) toString)
      )
    ]
    sec-str
  )
)

(defstruct entry :counter-val :time :thread-name)

(def counter (ref 0))
(def log (ref ()))


(defn do-loop 
"Runs the counter increment and state recording num-iterations times,
and returns a list of status struct result objects."
[num-iterations] 
  (loop [i num-iterations]
    (if (zero? i)
      nil
      (do
        (dosync
          (let [
            counter-val (alter counter inc)
            thread-name (str (Thread/currentThread))
            e (struct entry @counter (- (System/currentTimeMillis) reference-time) thread-name)
            ]
            (println e) (flush)
            (alter log conj e)
          )
        )
        (Thread/sleep 500)
        (recur (dec i))
      )
    )
  )
  (println (str (Thread/currentThread)) "done") (flush)
)


(defn create-loop-fn [times] (fn [] (do-loop times)))


(def conc-test (create-loop-fn 5))


(defn run-test [num-threads]
  (loop [i 0 threads ()]
    (if (= i num-threads)
      (do
        (println (map #(. % join) threads))
        (println "All " (count threads) " threads joined.")
        (println @log)
      )
      (let [t (Thread. conc-test)]
        (. t start)
        (recur (inc i) (conj threads t))
      )
    )
  )
  @log
)

(defn outer-test []
  (let [num-threads 5
        log (run-test num-threads)]
    (map #(println %) log)
  )
)


(outer-test)

