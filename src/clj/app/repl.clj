(ns app.repl
  (:import
   [java.lang String]
   [java.time Instant ZoneId LocalDateTime ZonedDateTime]

   [org.lmdbjava
    Env EnvFlags DirectBufferProxy Verifier ByteBufferProxy Txn
    SeekOp Dbi DbiFlags PutFlags]

   [app.types DbKey DbValue])
  (:require

   [taoensso.timbre :as timbre
    :refer [log  trace  debug  info  warn  error  fatal  report
            logf tracef debugf infof warnf errorf fatalf reportf
            spy get-env]]

   [app.verifier :refer [zone-id
                         options-datetime-range
                         ->local-date-time]]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn time-repl []
  (-> (Instant/parse "2016-01-01T14:30:00Z")
      (.getEpochSecond))

  (-> (second (options-datetime-range)) ->local-date-time)

  nil)

(defn count-repl []
  (debug (count (options-datetime-range)))
  (->> (options-datetime-range)
       (map (fn [x] [(.toEpochSecond x)
                     (.toString x)]))
       (map println)
       (dorun)))
(comment (count-repl))
