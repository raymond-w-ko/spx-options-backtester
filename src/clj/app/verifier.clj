(ns app.verifier
  (:import
   [java.lang String]
   [java.time Instant ZoneId LocalDateTime ZonedDateTime DayOfWeek]
   [java.time.temporal ChronoUnit]

   [org.lmdbjava
    Env EnvFlags DirectBufferProxy Verifier ByteBufferProxy Txn
    SeekOp Dbi DbiFlags PutFlags KeyRange]

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

   [app.utils :refer [iter-seq print-key]]
   [app.lmdb :as lmdb :refer [create-read-env open-db]]
   [clojure.string :as str]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def zone-id (ZoneId/of "US/Eastern"))

(defn ->local-date-time [instant]
  (LocalDateTime/ofInstant instant zone-id))

(def start-dt (-> (LocalDateTime/of 2016 1 1 9 31 00)
                  (ZonedDateTime/of zone-id)))
(def end-dt (-> (LocalDateTime/of 2021 1 1 9 31 00)
                (ZonedDateTime/of zone-id)))

(defn options-datetime-range
  ([]
   (options-datetime-range start-dt))
  ([t]
   (cons t
         (lazy-seq
          (let [next-t (.plus t 1 ChronoUnit/DAYS)]
            (cond
              (.equals next-t end-dt) nil
              (.isAfter next-t end-dt) nil
              :else (options-datetime-range next-t)))))))

(defn is-weekend? [t]
  (or (= (.getDayOfWeek t) DayOfWeek/SUNDAY)
      (= (.getDayOfWeek t) DayOfWeek/SATURDAY)))

(defn verify-db []
  (let [env (create-read-env "../cboe-options-interval-importer/dbs/spx")
        db (open-db env)
        txn (.txnRead env)

        ts (options-datetime-range)

        k (let [k (new DbKey)]
            k)]
    (loop [t (first ts)
           ts (rest ts)]
      (when t
        (when-not (is-weekend? t)
          (set! (.-quoteDateTime k) (.toInstant t))
          (let [key-buf (.toBuffer k)
                cursor (.iterate db txn (KeyRange/atLeast key-buf))
                xs (iter-seq cursor)]
            (let [k (-> (first xs)
                        (.key)
                        (DbKey/fromBuffer))
                  found-t (.-quoteDateTime k)
                  esec (.toEpochSecond t)]
              (when-not (or (= (+ 0 esec) (.getEpochSecond found-t)))
                (debug "target-t" (.toEpochSecond t) t
                       "found-t" (.getEpochSecond found-t) found-t
                       (.getDayOfWeek t))))))
        (recur (first ts) (rest ts))))
    (.close env)))
(comment (verify-db))
