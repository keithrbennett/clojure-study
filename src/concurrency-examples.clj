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
  (def thread-name (str (Thread/currentThread)))
  (loop [i num-iterations]
    (if (zero? i)
      nil
      (do
        (dosync
          (let [
            counter-val (alter counter inc)
            e (struct entry @counter (- (System/currentTimeMillis) reference-time) thread-name)
            ]
            (println e)
            (alter log conj e)
          )
        )
        (Thread/sleep 200)
        (recur (dec i))
      )
    )
  )
  (println "Done")
)

(defn conc-test [] (do-loop 3))

(. (Thread. conc-test) start)
