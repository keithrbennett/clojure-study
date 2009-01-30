; Simple Illustration of Clojure Concurrency
;
; Spawns multiple threads that each increment and log a counter multiple times
; with intervening sleeps.
;
; Keith Bennett (kbennett _at_ bbsinc _dot_ biz)


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
  (dotimes [_ num-iterations]

    ; There's a lot in this dosync; if this were a "real" production 
    ; application, some of this could be removed from the transaction,
    ; at the expense of a little more complexity (probably an outer 'let').
    (dosync
      (let [
        counter-val (alter counter inc)
        thread-name (str (Thread/currentThread))
        tm (- (System/currentTimeMillis) reference-time)
        entry (struct log-entry counter-val tm thread-name)
      ]
        (println (create-formatted-log-string counter-val tm))

        ; Conjoin the log entry to the list.  It will be added to the head of
        ; the list, so we will reverse the list later to preserve ascending
        ; chronological order.
        (alter log conj entry)
      )
    )

    ; The sleep is added here because without it, the first threads created may
    ; finish before the later threads are created.
    (Thread/sleep 50)
  )

  ; Put println in a transaction so the output from the multiple threads
  ; do not get interleaved.  The flush is there so that the string and its
  ; new line are output before the next print begins.  (Without the flush,
  ; every 5 or so runs resulted in twi entries on one line, and then an
  ; empty blank line.)
  (dosync (println (str "Thread done: "(Thread/currentThread))) flush)
)


(defn run-test 
"Spawns the threads that will each run do-loop, and returns the list of
resulting log entries."
[num-threads]
  (loop [i 0 threads ()]
    (if (< i num-threads)
      (let [
        f #(do-loop 5)
        t (Thread. f)
      ]
        (. t start)
        (recur (inc i) (conj threads t))
      )
      (do
        (println "Waiting for all spawned threads to finish...")
        (doall (map #(. % join) threads))
        (println "All " (count threads) " threads finished.")
      )
    )
  )
  @log
)

(defn outer-test []
  (let [num-threads 5
        log (run-test num-threads)
        log-copy (reverse log)
  ]
    (doseq [log-entry log-copy] (println log-entry))
  )
)


(outer-test)

