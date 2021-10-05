(ns app.backtester
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

   [app.macros :refer [->hash cond-xlet]]
   [app.utils :refer [iter-seq print-key]]
   [app.lmdb :as lmdb :refer [create-read-env open-db]]
   [clojure.string :as str]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn short-put-strategy [{:as args :keys [target-delta profit-target]}]
  (let []
    ))

(defn -main []
  (let [env (create-read-env "../cboe-options-interval-importer/dbs/spx")
        db (open-db env)
        txn (.txnRead env)
        
        args (->hash env db txn)]
    (short-put-strategy (merge
                         args
                         {:target-delta 0.08 :profit-target 0.7}))
    (.close env)))
(comment (-main))
