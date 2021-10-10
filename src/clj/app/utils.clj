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


(defn db-key-str [db-key]
  (->> [(.-quoteDateTime db-key)
        (str (.-root db-key))
        (.-optionType db-key)
        (.-expirationDate db-key)
        (.-strike db-key)]
       (interpose " ")
       (apply str)))

(defn print-db-key [db-key]
  (debug (db-key-str db-key)))

(defn db-value-str [db-value]
  (->> ["B" (format "%.2f" (.-bid db-value))
        "A" (format "%.2f" (.-ask db-value))
        "δ" (.-delta db-value)
        "O" (.-open db-value)
        "H" (.-high db-value)
        "L" (.-low db-value)
        "C" (.-close db-value)]
       (interpose " ")
       (apply str)))

(defn db-pair-str [[db-key db-value]]
  (str (db-key-str db-key) " - " (db-value-str db-value)))
