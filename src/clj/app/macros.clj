(ns app.macros)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro ->hash [& vars]
  (list `zipmap
    (mapv keyword vars)
    (vec vars)))

(defmacro args
  "Converts (args a b c) -> (assoc args :a a :b b :c c)"
  [& vars]
  (let [xs (interleave (mapv keyword vars)
                       (vec vars))]
    `(assoc ~'args ~@xs)))

(defmacro cond-let
  "An alternative to `clojure.core/cond` where instead of a test/expression pair, it is possible
  to have a :let/binding vector pair."
  [& clauses]
  (cond (empty? clauses)
        nil

        (not (even? (count clauses)))
        (throw (ex-info (str `cond-let " requires an even number of forms")
                        {:form &form
                         :meta (meta &form)}))

        :else
        (let [[test expr-or-binding-form & more-clauses] clauses]
          (if (= :let test)
            `(let ~expr-or-binding-form (cond-let ~@more-clauses))
            ;; Standard case
            `(if ~test
               ~expr-or-binding-form
               (cond-let ~@more-clauses))))))

(defmacro cond-xlet
  "An alternative to `clojure.core/cond` where instead of a test/expression pair,
  it is possible to also have:
  :do form  - unconditionally execute `form`, useful for printf style debugging or logging
  :let []   - standard let binding vector pair

  Try to use :let if you know that a function call result is synchronous."
  [& clauses]
  (cond
   (empty? clauses)
   nil

   (not (even? (count clauses)))
   (throw (ex-info (str `cond-xlet " requires an even number of forms")
                   {:form &form
                    :meta (meta &form)}))

   :else
   (let [[test expr-or-binding-form & more-clauses] clauses]
     (cond
      (= :let test) `(let ~expr-or-binding-form (cond-xlet ~@more-clauses))
      (= :do test) `(when true ~expr-or-binding-form (cond-xlet ~@more-clauses))
      (= :return-if test) `(if-let [x# ~expr-or-binding-form]
                             x#
                             (cond-xlet ~@more-clauses))
      ;; standard case
      :else `(if ~test
               ~expr-or-binding-form
               (cond-xlet ~@more-clauses))))))

