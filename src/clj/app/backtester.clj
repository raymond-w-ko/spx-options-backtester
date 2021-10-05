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

   [app.macros :as mac :refer [->hash cond-xlet]]
   [app.utils :refer [iter-seq print-db-key db-key-str db-value-str db-pair-str]]
   [app.lmdb :as lmdb :refer [create-read-env open-db]]
   [clojure.string :as str]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
(def zone-id (ZoneId/of "US/Eastern"))
(def margin-percent-of-notional 0.20)
(def option-fee-per-action 1.00)

(defn is-spwx-put-db-key? [pair]
  (let [key-buf (.key pair)
        db-key (DbKey/fromBuffer key-buf)]
    (and (= (.-root db-key) "SPXW ")
         (= (.-optionType db-key) \P) 
         db-key)))

(defn find-next-trading-day [{:as args :keys [log! db txn db-key]}]
  (let [key-buf (.toBuffer db-key)
        db-key (->> (iter-seq (.iterate db txn (KeyRange/atLeast key-buf)))
                    (some is-spwx-put-db-key?))
        t (.-quoteDateTime db-key)]
    (print-db-key db-key)
    (log! (str "advancing to t " t))
    (-> args
        (assoc :state :decide-next-state
               :t t
               :db-key db-key))))

(defn decide-next-state [{:as args :keys [log! positions]}]
  (cond-xlet
   (= 0 (count positions))
   (do (log!  "opening new position")
       (assoc args :state :open-new-position))))

(defn find-kv-with-target-delta [{:keys [target-delta]} xs]
  (letfn [(rf [coll pair]
            (let [v (DbValue/fromBuffer (.val pair))
                  dist (Math/abs (- (Math/abs (.-delta v)) target-delta))]
              (conj coll [dist (DbKey/fromBuffer (.key pair)) v])))]
    (->> (reduce rf [] xs)
         (apply (partial min-key first))
         (rest))))

(defn round-price [x]
  (double (/ (Math/round (* 100 x)) 100)))

(defn short-mid-price [args bid ask]
  (let [dist (- ask bid)]
    (round-price (+ bid (* dist 0.2)))))

(defn calc-short-qty [{:keys [balance]} db-key]
  (let [strike (.-strike db-key)]
    (- (int (/ balance (* strike 100 margin-percent-of-notional))))))

(defn short-to-open [{:as args :keys [log! t profit-target]} db-pair]
  (cond-xlet
   :let [[db-key db-value] db-pair
         bid (.-bid db-value)
         ask (.-ask db-value)]
   :do (log! "shorting option at time" (.toString t))
   :do (log! (db-pair-str db-pair))
   :let [qty (calc-short-qty args db-key)
         mid (short-mid-price args bid ask)
         credit (round-price (* (- qty) (- (* 100 mid) option-fee-per-action)))]
   :do (log! "mid price" mid)
   :do (log! "credit" credit)
   :let [pos {:qty qty
              :mid mid
              :expirationDate (.-expirationDate db-key)
              :target-close-price (round-price (* mid (- 1.0 profit-target)))
              :strike (.-strike db-key)}]
   :return
   (-> args
       (update :positions conj pos)
       (update :balance + credit))))

(defn open-new-position
  [{:as args :keys [log! db txn db-key]}]
  (cond-xlet
   :let [end-key (let [y (.clone db-key)]
                   (set! (.-strike y) Integer/MAX_VALUE)
                   y)
         xs (iter-seq (.iterate db txn (KeyRange/closed (.toBuffer db-key)
                                                        (.toBuffer end-key))))
         db-pair (find-kv-with-target-delta args xs)]
   :return (-> (short-to-open args db-pair)
               (assoc :state :exit))))

(defn short-put-strategy [{:as args :keys []}]
  (let [*logs (atom [])
        log! (fn [& more] (swap! *logs conj (->> (interpose " " more)
                                                 (apply str))))
        
        t (-> (LocalDateTime/of 2016 9 1 0 0 00)
              (ZonedDateTime/of zone-id))
        db-key (let [x (new DbKey)]
                 (set! (.-quoteDateTime x) t)
                 (set! (.-root x) "SPXW ")
                 (set! (.-optionType x) \P)
                 x)

        state :find-next-trading-day
        positions []
        balance 1000000
        args (mac/args *logs log! db-key t state positions balance)]

    (loop [{:as args :keys [state]} args]
      (cond-xlet
       (= state :decide-next-state) (recur (decide-next-state args))
       (= state :open-new-position) (recur (open-new-position args))
       (= state :find-next-trading-day) (recur (find-next-trading-day args))
       :else args))))

(defn write-backtest-result! [{:keys [*logs positions]}]
  (debug (pr-str positions))
  (spit "backtest.log" (->> (interpose "\n" @*logs)
                            (apply str))))

(defn -main []
  (let [env (create-read-env "../cboe-options-interval-importer/dbs/spx")
        db (open-db env)
        txn (.txnRead env)
        
        args (->hash env db txn)]
    (-> (short-put-strategy (merge args {:target-delta 0.08 :profit-target 0.7}))
        (write-backtest-result!))
    (.close env)))
(comment (-main))
