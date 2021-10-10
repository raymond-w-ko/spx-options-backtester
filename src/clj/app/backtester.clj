(ns app.backtester
  (:import
   [java.lang String]
   [java.time Instant
    ZoneId LocalDateTime ZonedDateTime DayOfWeek LocalDate Duration]
   [java.time.temporal ChronoUnit]

   [org.lmdbjava
    Env EnvFlags DirectBufferProxy Verifier ByteBufferProxy Txn
    SeekOp Dbi DbiFlags PutFlags KeyRange]

   [app.types DbKey DbValue Utils])
  (:require
   [fipp.edn :refer [pprint] :rename {pprint fipp}]
   [clojure.java.io :as io]
   [clojure.string :as str]
   [clojure.core.cache.wrapped :as cw]
   [clojure.core.async
    :as async
    :refer [go go-loop >! >!! <! <!! chan put! thread timeout close! to-chan
            pipeline pipeline-blocking]]
   
   [clj-async-profiler.core :as prof]
   [taoensso.timbre :as timbre
    :refer [log  trace  debug  info  warn  error  fatal  report
            logf tracef debugf infof warnf errorf fatalf reportf
            spy get-env]]

   [app.macros :as mac :refer [->hash cond-xlet]]
   [app.utils :refer [iter-seq print-db-key db-key-str db-value-str db-pair-str
                      ]]
   [app.lmdb :as lmdb :refer [create-read-env open-db]]
   [app.common :refer [some-db-key is-root+type? is-spx-put? is-spxw-put?
                       mean round-price sell-mid-price buy-mid-price
                       zone-id str->pm-exp-zdt  str->am-exp-zdt
                       next-day-market-open-time
                       find-kv-with-target-delta]]
   [clojure.string :as str]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(set! *warn-on-reflection* true)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def margin-percent-of-notional 0.20)
(def option-fee-per-action 1.00)

(defn log-balance! [{:as args :keys [log! log-balance! t balance]}
                    send-to-file]
  (let [balance (Math/floor balance)]
    (when send-to-file (log-balance! t balance))
    (log! "balance" balance))
  args)

(defn calc-short-qty [{:keys [balance]} ^DbKey db-key]
  (let [strike (.-strike db-key)
        ^float unrounded-qty (/ balance (* strike 100 margin-percent-of-notional))]
    (- (Math/round unrounded-qty))))

(defn get-db-value
  [{:keys [log! ^Dbi db txn ^DbKey db-key]}
   t expirationDate strike]
  (set! (.-quoteDateTime db-key) t)
  (set! (.-expirationDate db-key) expirationDate)
  (set! (.-strike db-key) strike)
  (let [vbuf (.get db txn (.toBuffer db-key))]
    (when-not vbuf
      (log! "missing value for" (db-key-str db-key)))
    (when vbuf
      (DbValue/fromBuffer vbuf))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn bump-to-next-day [{:as args :keys [log! ^ZonedDateTime t]}]
  (let [t (-> t
              (.plusDays 1)
              (.withHour 0)
              (.withMinute 0))]
    (log! "")
    (assoc args
           :t t
           :state :find-next-trading-time)))

(defn find-next-trading-time
  [{:as args :keys [log! ^Dbi db txn ^DbKey db-key]}]
  (let [key-buf (.toBuffer db-key)
        db-key (some-db-key args (KeyRange/atLeast key-buf) is-spxw-put?)
        t (.-quoteDateTime db-key)]
    (println t)
    (log! (str "advancing to t " t))
    (-> args
        (assoc :state :find-target-exp-date
               :t t
               :db-key db-key))))

(def market-closed-due-to-bush-death-date "2018-12-05")

(defn get-next-contract-date-of-root
  [{:as args :keys [^Dbi db txn ^DbKey db-key log!]} root]
  (let [start-key (let [^DbKey db-key (.clone db-key)]
                    (set! (.-root db-key) root)
                    db-key)
        end-key (let [^DbKey db-key (.clone db-key)
                      t (.-quoteDateTime start-key)
                      t-cap (.plusMinutes t 1)]
                  (set! (.-root db-key) root)
                  (set! (.-quoteDateTime db-key) t-cap)
                  db-key)
        ; _ (log! "looking for root" root)
        db-key (some-db-key
                args
                (KeyRange/closedOpen (.toBuffer start-key) (.toBuffer end-key))
                #(is-root+type? root \P %))]
    (when db-key
      (let [exp-date (.-expirationDate db-key)]
        (cond
         (= exp-date market-closed-due-to-bush-death-date) "2018-12-07"
         :else exp-date)))))

(defn decide-contract-type
  [^ZonedDateTime t spxw-date spx-date]
  (cond
   (nil? spx-date) spxw-date
   (nil? spxw-date) spx-date
   :else (let [t (.toLocalDate t)
               spxw-date (LocalDate/parse spxw-date)]
           (<= (.between ChronoUnit/DAYS t spxw-date) 4))))

(defn find-target-exp-date
  [{:as args :keys [log! db txn ^DbKey db-key ^ZonedDateTime t]}]
  (cond-xlet
   :do (log! "opening new position")
   :let [next-day-t (-> t
                        (.plusDays 1)
                        (.withHour 0)
                        (.withMinute 0))
         exp-date (-> next-day-t .toLocalDate .toString)
         db-key (do (set! (.-expirationDate db-key) exp-date)
                    db-key)
         args (mac/args db-key)
         spxw-exp-date (get-next-contract-date-of-root args "SPXW ")
         spx-exp-date (get-next-contract-date-of-root args "SPX  ")
         _ (log! "SPXW" spxw-exp-date "SPX" spx-exp-date)

         [target-root target-exp-date]
         (if (decide-contract-type t spxw-exp-date spx-exp-date)
           ["SPXW " spxw-exp-date]
           ["SPX  " spx-exp-date])]
   :do (log! "next DTE uses" target-root target-exp-date)

   :return (assoc args
                  :target-root target-root
                  :target-exp-date target-exp-date
                  :state :open-short-position)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn short-to-open [{:as args :keys [log! t target-root profit-target]}
                     db-pair]
  (cond-xlet
   :let [[^DbKey db-key ^DbValue db-value] db-pair
         bid (.-bid db-value)
         ask (.-ask db-value)]
   :do (log! "shorting option at time" (.toString t))
   :do (log! (db-pair-str db-pair))
   :do (assert (< 0 ask)  "zero or neg ask ")
   :let [qty (calc-short-qty args db-key)
         mid (sell-mid-price args bid ask)
         credit (round-price (* (- qty) (- (* 100 mid) option-fee-per-action)))
         target-close-price (round-price (* mid (- 1.0 profit-target)))]
   :do (log! "qty" qty)
   :do (log! "mid price" mid)
   :do (log! "credit" credit)
   :do (log! "target close price" target-close-price)
   :let [pos {:qty qty
              :mid mid
              :expirationDate (.-expirationDate db-key)
              :exp-dt (if (= "SPXW " target-root)
                            (str->pm-exp-zdt (.-expirationDate db-key))
                            (str->am-exp-zdt (.-expirationDate db-key)))
              :target-close-price target-close-price
              :strike (.-strike db-key)}]
   :return
   (-> args
       (assoc :position pos)
       (update :balance + credit))))

(defn open-new-position
  [{:as args :keys [^Dbi db txn ^DbKey db-key
                    target-delta target-root target-exp-date]}]
  (cond-xlet
   :let [start-key (do (set! (.-strike db-key) 0)
                    (set! (.-root db-key) target-root)
                    (set! (.-expirationDate db-key) target-exp-date)
                    db-key)
         end-key (let [^DbKey y (.clone db-key)]
                   (set! (.-strike y) Integer/MAX_VALUE)
                   y)
         db-pair (find-kv-with-target-delta args start-key end-key target-delta)]
   :return (-> (short-to-open args db-pair)
               (log-balance! false)
               (assoc :state :tick))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn buy-to-close
  [{:as args :keys [log! db-key position t db-value mid trigger-mid]}]
  (cond-xlet
   :do (log! "buy to close at" t)
   :do (log! (db-key-str db-key) "-" (db-value-str db-value))
   :let [{:keys [qty]} position
         debit (round-price (* (- qty) (+ (* 100 mid) option-fee-per-action)))]
   :do (log! "mid price" mid "trigger mid" trigger-mid)
   :do (log! "qty" (Math/abs ^float qty))
   :do (log! "debit" debit)
   :return
   (-> args
       (update :balance - debit)
       (assoc :position nil))))

(defn close-existing-position [{:as args}]
  (-> args
      (buy-to-close)
      (log-balance! true)
      (assoc :state :bump-to-next-day)))

(defn assignment
  [{:as args :keys [log! log-pipe! t ^DbValue db-value position bid ask]}]
  (cond-xlet
   :do (log! "ASSIGNMENT at " t)
   :let [au-price (.-active_underlying_price db-value)
         iu-price (.-implied_underlying_price db-value)]
   :do (log! "active price" au-price "implied price" iu-price)
   :do (log! "option bid" bid "option ask" ask)
   :let [{:keys [strike qty]} position
         assignment-price (if (<= strike au-price)
                            0.0
                            (round-price (- strike au-price)))
         debit (* 100 (Math/abs ^float qty) assignment-price)]
   :do (log! "assignment price" assignment-price)
   :return (-> args
               (update :balance - debit)
               (log-balance! true)
               (log-pipe! "")
               (assoc :state :time-travel-back-and-open))))

(defn time-travel-back-and-open
  [{:as args :keys [log! ^ZonedDateTime t ^DbKey db-key]}]
  (let [t (-> t
              (.withHour (+ 12 3))
              (.withMinute 30))]
    (log! "time travel backwards to" t)
    (set! (.-quoteDateTime db-key) t)
    (assoc args
           :t t
           :state :find-target-exp-date)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn update-current-pos-price [{:as args :keys [t position]}]
  (cond-xlet
   :let [{:keys [expirationDate strike]} position
         ^DbValue db-value (get-db-value args t expirationDate strike)
         bid (when db-value (.-bid db-value))
         ask (when db-value (.-ask db-value))
         high (when db-value (.-high db-value))
         low (when db-value (.-low db-value))
         mid (when db-value (buy-mid-price args bid ask))
         trigger-mid (when db-value (round-price (mean bid ask)))]
   :return (mac/args bid ask mid trigger-mid high low
                     db-value)))

(defn profit-target-reached?
  [{:as args :keys [log! ^ZonedDateTime t position ^DbValue db-value
                    bid ask mid high low trigger-mid]}]
  (cond-xlet
   ;; do not close if after 3:55PM
   :let [latest-close-time (-> t (.withHour (+ 12 3)) (.withMinute 55))]
   (.isAfter t latest-close-time) false

   :let [{:keys [target-close-price]} position]

   ;; bad data
   (= 0.0 (.-delta db-value)) false
   (not (< bid ask)) (do (log! "BAD QUOTE" t (db-value-str db-value))
                         false)
   (and (= bid 0.0) (= ask 0.0)) (do (log! "BAD QUOTE" t (db-value-str db-value))
                                     false)
   
   (and (<= trigger-mid target-close-price)) true
   (and (<= bid ask target-close-price)) true
   
   :else false))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn tick
  [{:as args
    :keys [^ZonedDateTime t position db-key log!]}]
  (cond-xlet
   :let [{:keys [exp-dt]} position]

   (or (.isEqual t exp-dt) (.isAfter t exp-dt))
   (assoc args :state :assignment)

   ;; if 4:16PM, skip to 9:30AM, and loop will increment to 9:31AM
   (and (= (+ 12 4) (.getHour t)) (= 16 (.getMinute t)))
   (assoc args :t (next-day-market-open-time t) :state :tick)

   :let [args (update-current-pos-price args)]
   ;; market is probably closed on this day
   (= nil (:mid args))
   (assoc args :t (next-day-market-open-time t) :state :tick)

   ;; check if profit target reached
   (profit-target-reached? args) (assoc args :state :close-short-position)

   ;; else tick time along
   :else
   (-> args
       (update :t (fn [^ZonedDateTime t] (.plusMinutes t 1)))
       (assoc :state :tick))))

(defn write-backtest-result! [{:keys [*balances *logs]}]
  (spit "balances.log" (->> (interpose "\n" @*balances)
                            (apply str)))
  (spit "backtest.log" (->> (interpose "\n" @*logs)
                            (apply str))))

(defn short-put-strategy [{:as args :keys []}]
  (let [*logs (atom [])
        *balances (atom [])
        log! (fn [& more] (swap! *logs conj (->> (interpose " " more)
                                                 (apply str))))
        log-balance! (fn [& more] (swap! *balances
                                         conj
                                         (->> (interpose " " more) (apply str))))
        log-pipe! (fn [args & more]
                    (apply log! more)
                    args)

        t (-> (LocalDateTime/of 2016 9 1 0 0 00)
              (ZonedDateTime/of zone-id))
        db-key (let [x (new DbKey)]
                 (set! (.-quoteDateTime x) t)
                 (set! (.-root x) "SPXW ")
                 (set! (.-optionType x) \P)
                 x)

        state :find-next-trading-time
        balance 1000000
        args (mac/args *logs *balances log! log-balance! log-pipe!
                       db-key t state balance)]

    (try
     (loop [{:as args :keys [state]} args]
       (cond-xlet
        (= state :bump-to-next-day) (recur (bump-to-next-day args))
        (= state :find-next-trading-time) (recur (find-next-trading-time args))
        (= state :find-target-exp-date) (recur (find-target-exp-date args))
        (= state :tick) (recur (tick args))
        (= state :open-short-position) (recur (open-new-position args))
        (= state :close-short-position) (recur (close-existing-position args))
        (= state :assignment) (recur (assignment args))
        (= state :time-travel-back-and-open) (recur (time-travel-back-and-open args))
        :else args))
     (finally
      (write-backtest-result! (->hash *balances *logs))))))

(defn run []
  (with-open [env (create-read-env "../cboe-options-interval-importer/dbs/spx")]
    (let [db (open-db env)
          txn (.txnRead env)

          args (->hash env db txn)]
      (debug "INIT")
      (short-put-strategy (merge args {:target-delta -0.08 :profit-target 0.7}))
      (.close env))))

(defn -main []
  (do
    (run))
  nil)
(comment (-main))
