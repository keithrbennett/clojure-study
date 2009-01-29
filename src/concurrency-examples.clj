; Simple Illustration of Clojure Concurrency

(ns biz.bbsinc.clojure-study
  (:import 
    (java.text DateFormat)
    (java.util Date)
  )
)


(defn create-log-entry []
  (let 
    [
      date-formatter (DateFormat/getDateTimeInstance)
      date-str (. date-formatter format (Date.))
    ]
    (format "%s" date-str)
  )
)

