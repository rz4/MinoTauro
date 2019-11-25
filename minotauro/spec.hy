;;; spec.hy
;; Updated: 11/23/19
;; File defines specfication system used for minotauro. Inspired from clojure's spec.
;;
;; Design Principles:
;;
;; - Spec should function soley as a macro-imported system.
;;
;; To use macros, import using:
;; (require [minotauro.spec [*]])

; Imports
(import hy)

; Macros
(require [hy.contrib.walk [let]])

;-----SPEC------

;; Specifications Registry
(defn spec/registry []
  (global spec-registry)
  (try spec-registry
    (except [] (setv spec-registry {}) spec-registry)))

;; Specifications Registry
(defn spec/gen-registry []
  (global gen-spec-registry)
  (try gen-spec-registry
    (except [] (setv gen-spec-registry {}) gen-spec-registry)))

;; Conformed Specification Temporary Registry
(defn _spec/conform-registry [&optional [reset False]]
  (global conform-registry)
  (try (if reset
           (do (setv conform-registry {}) conform-registry)
           conform-registry)
    (except [] (setv conform-registry {}) conform-registry)))

;; Specification Evaluator
(defn _spec/eval [spec data &optional]
  (setv out ((get (spec/registry) spec) data))
  (assoc (_spec/conform-registry) spec out)
  out)

;;---Definintions----

;; Defines new specfication in registry
(defmacro spec/def [&rest args]
  (setv args (partition args :n 2))
  (setv registers [])
  (for [(, kw-namespace predicate) args]
    (setv predicate (macroexpand predicate))
    (.append registers `(assoc (spec/registry) ~kw-namespace ~predicate)))
  (assert (keyword? kw-namespace) "spec/ArgumentError: First argument must be a HyKeyword.")
  `(do (import [minotauro.spec [spec/registry]])
       ~@registers))

;; Defines new specfication generator in registry
(defmacro spec/defgen [kw-namespace args &rest body]
  (assert (keyword? kw-namespace) "spec/ArgumentError: First argument must be a HyKeyword.")
  `(do (import [minotauro.spec [spec/gen-registry]])
       (assoc (spec/gen-registry) ~kw-namespace (fn ~args ~@body))))

;;-----Rational Expressions-----

;; Nand Operator
(defmacro spec/nand [&rest specs]
  (setv fetchers [])
  (setv setter '(setv))
  (for [spec specs]
    (setv spec (macroexpand spec))
    (if (keyword? spec)
      (.append fetchers `(_spec/eval (quote ~spec) x))
      (if (instance? HyExpression spec)
        (do (setv var (gensym))
            (+= setter `(~var ~spec))
            (.append fetchers `(~var x)))
        (.append fetchers `(~spec x)))))
  `(do (import [minotauro.spec [_spec/eval]])
       ~setter
       (fn [x] (not (and ~@fetchers)))))

;; And Operator
(defmacro spec/and [&rest specs]
  (setv fetchers [])
  (setv setter '(setv))
  (for [spec specs]
    (setv spec (macroexpand spec))
    (if (keyword? spec)
      (.append fetchers `(_spec/eval (quote ~spec) x))
      (if (instance? HyExpression spec)
        (do (setv var (gensym))
            (+= setter `(~var ~spec))
            (.append fetchers `(~var x)))
        (.append fetchers `(~spec x)))))
  `(do (import [minotauro.spec [_spec/eval]])
       ~setter
       (fn [x] (and ~@fetchers))))

;; Or Operator
(defmacro spec/or [&rest specs]
  (setv fetchers [])
  (setv setter '(setv))
  (for [spec specs]
    (setv spec (macroexpand spec))
    (if (keyword? spec)
      (.append fetchers `(_spec/eval (quote ~spec) x))
      (if (instance? HyExpression spec)
        (do (setv var (gensym))
            (+= setter `(~var ~spec))
            (.append fetchers `(~var x)))
        (.append fetchers `(~spec x)))))
  `(do (import [minotauro.spec [_spec/eval]])
       ~setter
       (fn [x] (| ~@fetchers))))

;;
;(defmacro spec/keys [&rest args]

;; Dictionary of keys and vals
(defmacro spec/dict-of [keys vals]
  (setv keys (macroexpand keys)
        vals (macroexpand vals))
  (setv setter '(setv))
  (setv kfetcher (if (keyword? keys)
                     `(_spec/eval (quote ~keys) x)
                     (if (instance? HyExpression keys)
                       (do (setv var (gensym))
                           (+= setter `(~var ~keys))
                           `(~var x))
                       `(~keys x))))
  (setv vfetcher (if (keyword? vals)
                     `(_spec/eval (quote ~vals) x)
                     (if (instance? HyExpression vals)
                       (do (setv var (gensym))
                           (+= setter `(~var ~vals))
                           `(~var x))
                       `(~vals x))))
  `(do (import [minotauro.spec [_spec/eval]])
       ~setter
       (fn [dict-x] (and (not (some zero? (lfor x (.keys dict-x) ~kfetcher)))
                         (not (some zero? (lfor x (.values dict-x) ~vfetcher)))))))

;; Collection of spec
(defmacro spec/coll-of [spec]
  (setv spec (macroexpand spec))
  (setv setter '(setv))
  (setv fetcher (if (keyword? spec
                     `(_spec/eval (quote ~spec) x)
                     (if (instance? HyExpression spec)
                       (do (setv var (gensym))
                           (+= setter `(~var ~spec))
                           `(~var x))
                       `(~spec x)))))
  `(do (import [minotauro.spec [_spec/eval]])
       ~setter
       (fn [col-x] (not (some zero? (lfor x col-x ~fetcher))))))

;;
;;(defmacro spec/cat [&rest specs])

;;
(defmacro spec/components [&rest args]
  (setv args (partition args :n 2))
  (setv fetchers [])
  (setv setter '(setv))
  (for [(, key spec) args]
    (setv spec (macroexpand spec))
    (if (keyword? spec)
      (setv spec-check `(_spec/eval (quote ~spec) (get env (quote ~key))))
      (if (instance? HyExpression spec)
        (do (setv var (gensym))
            (+= setter `(~var ~spec))
            (setv spec-check `(~var (get env (quote ~key)))))
        (setv spec-check `(~spec (get env (quote ~key))))))
    (.append fetchers `(if (in (quote ~key) env) ~spec-check False)))
  `(do (import [minotauro.spec [_spec/eval]])
       ~setter
       (fn [x]
         (try (setv env (. x __dict__))
           (except [] (return False)))
         (and ~@fetchers))))


;;---Operators---

;; Check if data is valid according to spec
(defmacro spec/valid? [spec data]
  (setv spec (macroexpand spec))
  (setv setter '(setv))
  (setv fetcher (if (keyword? spec)
                  `(_spec/eval (quote ~spec) ~data)
                  (if (instance? HyExpression spec)
                    (do (setv var (gensym))
                        (+= setter `(~var ~spec))
                        `(~var ~data))
                    `(~spec ~data))))
  `(do (import [minotauro.spec [_spec/eval]])
       ~setter
       ~fetcher))

;; Pass data if valid according to spec
(defmacro spec/conform [spec data]
  (setv spec (macroexpand spec))
  (setv setter '(setv))
  (setv fetcher (if (keyword? spec)
                  `(_spec/eval (quote ~spec) ~data)
                  (if (instance? HyExpression spec)
                    (do (setv var (gensym))
                        (+= setter `(~var ~spec))
                        `(~var ~data))
                    `(~spec ~data))))
  `(do (import [minotauro.spec [_spec/eval]])
       ~setter
       (if ~fetcher
         ~data
         (assert False "spec/conform: Data does not conform to spec."))))

;; Return dictionary of valid? results on data according to spec
(defmacro spec/explain [spec data]
  (setv spec (macroexpand spec))
  (setv setter '(setv))
  (setv expr (if (keyword? spec)
                 `(_spec/eval (quote ~spec) ~data)
                 (if (instance? HyExpression spec)
                   (do (setv var (gensym))
                       (+= setter `(~var ~spec))
                       `(~var ~data))
                   `(~spec ~data))))
  `(do (import [minotauro.spec [_spec/eval _spec/conform-registry]])
       (_spec/conform-registry :reset True)
       ~setter
       ~expr
       (_spec/conform-registry)))

;; Return dictionary of portions of data which conform to spec
;;(defmacro spec/describe [spec data])

;-----Generation-----

;; Specification Generator
(defmacro spec/gen [spec &rest args]
 (setv spec (macroexpand spec))
 (setv setter '(setv))
 (setv fetcher (if (keyword? spec)
                 ((get (spec/gen-registry) spec) data)
                 (if (instance? HyExpression spec)
                   (do (setv var (gensym))
                       (+= setter `(~var ~spec))
                       `(~var ~data))
                   `(~spec ~data))))
 (macroexpand `(do (setv data (~gen-fetcher ~@args))
                   (spec/conform `spec data))))
