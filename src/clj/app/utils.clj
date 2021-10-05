(ns app.utils
  (:import
   [java.util.zip ZipInputStream]
   [java.io LineNumberReader InputStreamReader BufferedReader]
   [java.sql DriverManager Connection Statement PreparedStatement ResultSet])
  (:require
   [taoensso.timbre :as timbre
    :refer [log  trace  debug  info  warn  error  fatal  report
            logf tracef debugf infof warnf errorf fatalf reportf
            spy get-env]]
   [clojure.core.async
    :as async
    :refer [go go-loop >! <! chan put! alts! close! timeout]]
   [clojure.string :as str]
   [clojure.core.cache.wrapped :as cw]
   [clojure.java.io :as io]

   [app.macros :refer [cond-let]]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn iter-seq
  ([iterable] 
    (iter-seq iterable (.iterator iterable)))
  ([iterable i] 
    (lazy-seq 
      (when (.hasNext i)
        (cons (.next i) (iter-seq iterable i))))))


(defn print-key [db-key]
  (debug "timestamp" (.getEpochSecond (.-quoteDateTime db-key))
         "root" (.-root db-key)
         "optionType" (.-optionType db-key)
         "expirationDate" (.-expirationDate db-key)
         "strike" (.-strike db-key)))