(ns brave.core)

(defn get-raw-quote []
  (slurp "http://www.braveclojure.com/random-quote"))

(defn parse-quote [raw-quote]
  (-> raw-quote
      (clojure.string/split #"--")
      first                                                 ; discard author
      clojure.string/trim))

(defn get-quote []
  (-> (get-raw-quote) parse-quote))

(defn get-words [quote]
  (-> quote
      clojure.string/lower-case
      (clojure.string/replace #"[^ a-z]*" "")               ; removes punctuation from quote
      (clojure.string/split #" ")))

(defn histogramize [histogram words]
  (reduce
    (fn [histogram word]
      (update histogram word (fnil inc 0)))                 ; when val is nil, replace it with 0, then increment
    histogram
    words))


(defn quote-word-count [number-of-quotes]
  "Returns a histogram of the word counts in `number-of-quotes`
   quotes retrieved from http://www.braveclojure.com/random-quote"
  (let [word-count (atom {})]
    (doall
      (pmap
       (fn [_]
         (swap! word-count
                (fn [histogram]
                  (histogramize histogram (-> (get-quote)
                                              get-words)))))
       (range number-of-quotes)))
    @word-count))
