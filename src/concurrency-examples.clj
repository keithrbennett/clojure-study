; Simple Illustration of Clojure Concurrency


(ns concurrency-example)

; Times logged will be expressed in milliseconds after this time.
(def reference-time (System/currentTimeMillis))


(defn create-formatted-log-string
"Formats a single line string containing info about an access to counter."
[counter-val msec-elapsed]
  (let 
    [
      sec-str (format "Counter value: %10d   Time: %12.3f sec    Thread: %s"
          (long counter-val)
          (double (/ msec-elapsed 1000))
          (. (Thread/currentThread) toString)
      )
    ]
    sec-str
  )
)


(defstruct log-entry :counter-val :time :thread-name)


; This is the thing whose concurrent access will be demonstrated.
(def counter (ref 0))


; Will contain a list of log entries relating to accesses to counter.
(def log (ref ()))


(defn do-loop 
"Runs the counter increment test and its logging num-iterations times
on a single thread."
[num-iterations] 
  (dotimes [i num-iterations]
    (do
      (dosync
        (let [
          counter-val (alter counter inc)
          thread-name (str (Thread/currentThread))
          tm (- (System/currentTimeMillis) reference-time)
          e (struct log-entry counter-val tm thread-name)
        ]
          (println (create-formatted-log-string counter-val tm)) (flush)
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


(defn run-test [num-threads]
  (loop [i 0 threads ()]
    (if (= i num-threads)
      (do
        (println (map #(. % join) threads))
        (println "All " (count threads) " threads joined.")
        (println @log)
      )
      (let [
        f #(do-loop 5)
        t (Thread. f)]
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

