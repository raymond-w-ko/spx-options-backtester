(ns app.common
  (:import
   [java.lang String]
   [java.time Instant ZoneId LocalDateTime ZonedDateTime DayOfWeek]
   [java.time.temporal ChronoUnit]

   [org.lmdbjava
    Env EnvFlags DirectBufferProxy Verifier ByteBufferProxy Txn
    SeekOp Dbi DbiFlags PutFlags KeyRange CursorIterable]

   [app.types DbKey DbValue])
  (:require
   [fipp.edn :refer [pprint] :rename {pprint fipp}]
   [clojure.java.io :as io]
   [clojure.string :as str]
   [clojure.core.cache.wrapped :as cw]
   [clojure.core.async
    :as async
    :refer [go go-loop >! >!! <! <!! chan put! thread timeout close! to-chan
            pipeline pipeline-blocking]]

   [taoensso.timbre :as timbre
    :refer [log  trace  debug  info  warn  error  fatal  report
            logf tracef debugf infof warnf errorf fatalf reportf
            spy get-env]]

   [app.macros :as mac :refer [->hash cond-xlet]]
   [app.utils :refer [iter-seq print-db-key db-key-str db-value-str db-pair-str]]
   [app.lmdb :as lmdb :refer [create-read-env open-db]]
   [clojure.string :as str]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(set! *warn-on-reflection* true)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn round-price [x]
  (float (/ (Math/round ^float (* 100 x)) 100)))

(defn mean [a b]
  (/ (+ a b) 2.0))

(defn sell-mid-price
  "mid price to use when selling an option"
  [args bid ask]
  (round-price (- (mean bid ask) 0.03)))

(defn buy-mid-price
  "mid price to use when buying an option"
  [args bid ask]
  (round-price (+ (mean bid ask) 0.03)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def zone-id (ZoneId/of "US/Eastern"))
(defn str->pm-exp-zdt [^String exp-date]
  (let [year (-> (subs exp-date 0 4) Integer/parseInt)
        month (-> (subs exp-date 5 7) Integer/parseInt)
        day (-> (subs exp-date 8 10) Integer/parseInt)]
    (ZonedDateTime/of year month day (+ 12 4) 15 0 0
                      zone-id)))
(defn str->am-exp-zdt [^String exp-date]
  (let [year (-> (subs exp-date 0 4) Integer/parseInt)
        month (-> (subs exp-date 5 7) Integer/parseInt)
        day (-> (subs exp-date 8 10) Integer/parseInt)]
    (ZonedDateTime/of
     year month day 9 31 0 0
     zone-id)))

(defn next-day-market-open-time [^ZonedDateTime t]
  (-> t
      (.plusDays 1)
      (.withHour 9)
      (.withMinute 31)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn is-root+type? [^String root
                     ^Character option-type
                     ^org.lmdbjava.CursorIterable$KeyVal pair]
  (let [key-buf (.key pair)
        db-key (DbKey/fromBuffer key-buf)]
    (and (= (.-root db-key) root)
         (= (.-optionType db-key) option-type)
         db-key)))

(defn is-spx-put? [pair] (is-root+type? "SPX  " \P pair))
(defn is-spxw-put? [pair] (is-root+type? "SPXW " \P pair))

(defn ^DbKey some-db-key
  [{:keys [^Dbi db txn log!]} key-range pred]
  (with-open [cursor (.iterate db txn key-range)]
    (let [iter (.iterator cursor)]
      (loop [^org.lmdbjava.CursorIterable$KeyVal pair (and (.hasNext iter)
                                                           (.next iter))
             i 0]
        ; (when (= i 0) (log! "FIRST" (db-key-str (DbKey/fromBuffer (.key pair)))))
        (if (pred pair)
          (do (comment log! "iterated over" i "entries")
              (DbKey/fromBuffer (.key pair)))
          (when (.hasNext iter)
            (recur (.next iter) (inc i))))))))

(defn dist [a b]
  (Math/abs ^float (- a b)))

(defn find-kv-with-target-delta
  [{:keys [^Dbi db txn]}
   ^DbKey start-key
   ^DbKey end-key
   target-delta]
  (let [key-range (KeyRange/closed (.toBuffer start-key) (.toBuffer end-key))]
    (with-open [cursor (.iterate db txn key-range)]
      (let [iter (.iterator cursor)
            ]
        (->> (loop [^org.lmdbjava.CursorIterable$KeyVal pair (and (.hasNext iter)
                                                                  (.next iter))
                    pairs (transient [])]
               (let [db-value (DbValue/fromBuffer (.val pair))
                     delta (.-delta db-value)
                     di (dist target-delta delta)
                     pairs
                     (if (and (not= delta 0) (<= di 0.05))
                       (conj! pairs [di (DbKey/fromBuffer (.key pair)) db-value])
                       pairs)]
                 (if (.hasNext iter)
                   (recur (.next iter) pairs)
                   (persistent! pairs))))
             (apply (partial min-key first))
             (rest))))))
