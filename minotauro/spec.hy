;;; spec.hy
;; Updated: 11/14/19
;; File defines specfication system used for minotauro. Inspired from clojure's spec.
;;
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

;;
(defn spec/conform-registry [&optional [reset False]]
  (global conform-registry)
  (try (if reset
           (do (setv conform-registry {}) conform-registry)
           conform-registry)
    (except [] (setv conform-registry {}) conform-registry)))

;;
(defn spec/eval [spec data &optional]
  (setv out ((get (spec/registry) spec) data))
  (assoc (spec/conform-registry) spec out)
  out)

;; Assertion Flag Getter
(defn spec/asserts-flag []
  (global check-asserts-)
  (try check-asserts-
    (except [] (setv check-asserts- False) check-asserts-)))

;; Turn Assertions On or Off
(defn spec/check-asserts [boolean]
  (global check-asserts-)
  (setv check-asserts- boolean))

;;---Definintions----

;; Defines new specfication in registry
(defmacro spec/def [kw-namespace predicate]
  (setv predicate (macroexpand predicate))
  (assert (keyword? kw-namespace) "spec/ArgumentError: First argument must be a HyKeyword.")
  `(do (import [minotauro.spec [spec/registry]])
       (assoc (spec/registry) ~kw-namespace ~predicate)))

;;
(defmacro spec/nand [&rest specs]
  (setv fetchers [])
  (setv setter '(setv))
  (for [spec specs]
    (setv spec (macroexpand spec))
    (if (keyword? spec)
      (.append fetchers `(spec/eval (quote ~spec) x))
      (if (instance? HyExpression spec)
        (do (setv var (gensym))
            (+= setter `(~var ~spec))
            (.append fetchers `(~var x)))
        (.append fetchers `(~spec x)))))
  `(do (import [minotauro.spec [spec/eval]])
       ~setter
       (fn [x] (not (and ~@fetchers)))))

;;
(defmacro spec/and [&rest specs]
  (setv fetchers [])
  (setv setter '(setv))
  (for [spec specs]
    (setv spec (macroexpand spec))
    (if (keyword? spec)
      (.append fetchers `(spec/eval (quote ~spec) x))
      (if (instance? HyExpression spec)
        (do (setv var (gensym))
            (+= setter `(~var ~spec))
            (.append fetchers `(~var x)))
        (.append fetchers `(~spec x)))))
  `(do (import [minotauro.spec [spec/eval]])
       ~setter
       (fn [x] (and ~@fetchers))))

;;
(defmacro spec/or [&rest specs]
  (setv fetchers [])
  (setv setter '(setv))
  (for [spec specs]
    (setv spec (macroexpand spec))
    (if (keyword? spec)
      (.append fetchers `(spec/eval (quote ~spec) x))
      (if (instance? HyExpression spec)
        (do (setv var (gensym))
            (+= setter `(~var ~spec))
            (.append fetchers `(~var x)))
        (.append fetchers `(~spec x)))))
  `(do (import [minotauro.spec [spec/eval]])
       ~setter
       (fn [x] (| ~@fetchers))))

;;
;(defmacro spec/keys [&optional [req None] [opt None]])

;;
;;(defmacro spec/merge [])

;;
(defmacro spec/dict-of [keys vals]
  (setv keys (macroexpand keys)
        vals (macroexpand vals))
  (setv setter '(setv))
  (setv kfetcher (if (keyword? keys)
                     `(spec/eval (quote ~keys) x)
                     (if (instance? HyExpression keys)
                       (do (setv var (gensym))
                           (+= setter `(~var ~keys))
                           `(~var x))
                       `(~keys x))))
  (setv vfetcher (if (keyword? vals)
                     `(spec/eval (quote ~vals) x)
                     (if (instance? HyExpression vals)
                       (do (setv var (gensym))
                           (+= setter `(~var ~vals))
                           `(~var x))
                       `(~vals x))))
  `(do (import [minotauro.spec [spec/eval]])
       ~setter
       (fn [dict-x] (and (not (some zero? (lfor x (.keys dict-x) ~kfetcher)))
                         (not (some zero? (lfor x (.values dict-x) ~vfetcher)))))))

;;
(defmacro spec/coll-of [spec]
  (setv spec (macroexpand spec))
  (setv setter '(setv))
  (setv fetcher (if (keyword? spec
                     `(spec/eval (quote ~spec) x)
                     (if (instance? HyExpression spec)
                       (do (setv var (gensym))
                           (+= setter `(~var ~spec))
                           `(~var x))
                       `(~spec x)))))
  `(do (import [minotauro.spec [spec/eval]])
       ~setter
       (fn [col-x] (not (some zero? (lfor x col-x ~fetcher))))))

;;
;; (defmacro spec/elements [&rest specs]
;;   (setv fetchers [])
;;   (for [(, i spec) (enumerate specs)]
;;     (if (keyword? spec)
;;       (.append fetchers `((get (spec/registry) ~spec) (get x ~i)))
;;       (.append fetchers `(~spec (get x ~i)))))
;;   (setv nb-specs (len fetchers))
;;   `(do (import [minotauro.spec [spec/registry]])
;;        (fn [x] (and (= (len x) ~nb-specs) ~@fetchers))))

;;
;;(defmacro spec/cat [&rest specs])

;;
;; (defmacro spec/fdef [namespace-kw &optional [args None] [ret None]])

;;---Operators---
;;
(defmacro spec/valid? [spec data]
  (setv spec (macroexpand spec))
  (setv setter '(setv))
  (setv fetcher (if (keyword? spec)
                  `(spec/eval (quote ~spec) ~data)
                  (if (instance? HyExpression spec)
                    (do (setv var (gensym))
                        (+= setter `(~var ~spec))
                        `(~var ~data))
                    `(~spec ~data))))
  `(do (import [minotauro.spec [spec/eval]])
       ~setter
       ~fetcher))

;;
;; (defmacro spec/assert [spec data])

;;
(defmacro spec/conform [spec data]
  (setv spec (macroexpand spec))
  (setv setter '(setv))
  (setv fetcher (if (keyword? spec)
                  `(spec/eval (quote ~spec) ~data)
                  (if (instance? HyExpression spec)
                    (do (setv var (gensym))
                        (+= setter `(~var ~spec))
                        `(~var ~data))
                    `(~spec ~data))))
  `(do (import [minotauro.spec [spec/eval]])
       ~setter
       (if ~fetcher
         ~data
         (assert False "spec/conform: Data does not conform to spec."))))

;;
(defmacro spec/explain [spec data]
  (setv spec (macroexpand spec))
  (setv setter '(setv))
  (setv expr (if (keyword? spec)
                 `(spec/eval (quote ~spec) ~data)
                 (if (instance? HyExpression spec)
                   (do (setv var (gensym))
                       (+= setter `(~var ~spec))
                       `(~var ~data))
                   `(~spec ~data))))
  `(do (import [minotauro.spec [spec/eval spec/conform-registry]])
       (spec/conform-registry :reset True)
       ~setter
       ~expr
       (spec/conform-registry)))

;;
(defmacro spec/regex? [regex]
  `(do (import re)
       (let [regexp (re.compile ~regex)]
         (fn [x] (if (.search regexp x) True False)))))


;-----PREDICATES------
