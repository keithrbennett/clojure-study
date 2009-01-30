; Simple Illustration of Clojure Concurrency
;
; Spawns multiple threads that each increment and log a counter multiple times
; with intervening sleeps.
;
; Keith Bennett (kbennett _at_ bbsinc _dot_ biz)


(ns concurrency-example)


(defstruct log-entry :counter-val :time :thread-name)


; This is the thing whose concurrent access will be demonstrated.
(def counter (ref 0))


; Will contain a list of log entries relating to accesses to counter.
(def log (ref ()))


; Times logged will be expressed in milliseconds after this time.
(def reference-time (System/currentTimeMillis))


(defn create-formatted-log-string
"Formats a single line string containing info about an access to counter."
[log-entry]
  (let 
    [
      sec-str (format "Counter value: %10d   Time: %7.3f sec   %s"
          (long (log-entry :counter-val))
          (double (/ (log-entry :time) 1000))
          (log-entry :thread-name)
      )
    ]
    sec-str
  )
)


(defn do-loop-thread-function
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
        (println (create-formatted-log-string entry))

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
  ; every 5 or so runs resulted in two entries on one line, and then an
  ; empty blank line.)
  (dosync (println (str "Thread done: "(Thread/currentThread))) flush)
)


(defn run-test-multiple-threads
"Spawns the threads that will each run do-loop-thread-function, and returns
the list of resulting log entries."
[num-threads tries-per-thread]
  (loop [i 0 threads ()]
    (if (< i num-threads)

      ; Start a thread running do-loop-thread-function, and record the thread
      ; in a list so that we can 'join' it later.
      (let [
        f #(do-loop-thread-function tries-per-thread)
        t (Thread. f)
      ]
        (. t start)
        (recur (inc i) (conj threads t))
      )

      ; All threads have now been started.
      ; The join will cause this main thread to block until all
      ; spawned threads are done.
      (do
        (println "Waiting for all spawned threads to finish...")
        (doall (map #(. % join) threads))
        (println "All " (count threads) " threads finished.")
      )
    )
  )
  (reverse @log)
)


(defn check-results
"Checks the results to ensure that they are correct."
[log num-threads tries-per-thread]

  (assert (not (nil? log)))
  (println "Passed: results not null")

  (assert (= (count log) (* num-threads tries-per-thread)))
  (println "Passed: result set is correct size")

  (let [
    counter-vals (map #(:counter-val %) log)
  ]
    (assert (= (count counter-vals) (count (distinct counter-vals))))
    (println "Passed: all values are distinct.")
  )
)


(defn main 
"Program entry point that runs the test."
[]
  (let [
    num-threads 5
    tries-per-thread 5
    log (run-test-multiple-threads num-threads tries-per-thread)
  ]
    (doseq [log-entry log] (println log-entry))
    (check-results log num-threads tries-per-thread)
  )
)


(main)
