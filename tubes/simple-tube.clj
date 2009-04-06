(defn get-props-keys [] (.. System getProperties stringPropertyNames))
(defn seq-upper [seq] (map #(.toUpperCase %) seq))
(defn outputter [seq] (dorun (map #(println %) seq)))

;; (outputter (seq-upper (get-props-keys)))

(def my-tube [get-props-keys seq-upper outputter])


(defn do-tube [tube]
  (if (or (nil? tube) (empty? tube)) nil
    (loop [
      result ((first tube))
      tube (rest tube)
    ]
      (let [f1 (first tube)]
        (if f1 (recur (f1 result) (rest tube)) result)))))
      
(do-tube my-tube)
