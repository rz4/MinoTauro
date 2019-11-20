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
      (.append fetchers `((get (spec/registry) ~spec) x))
      (if (instance? HyExpression spec)
        (do (setv var (gensym))
            (+= setter `(~var ~spec))
            (.append fetchers `(~var x)))
        (.append fetchers `(~spec x)))))
  `(do (import [minotauro.spec [spec/registry]])
       ~setter
       (fn [x] (not (and ~@fetchers)))))

;;
(defmacro spec/and [&rest specs]
  (setv fetchers [])
  (setv setter '(setv))
  (for [spec specs]
    (setv spec (macroexpand spec))
    (if (keyword? spec)
      (.append fetchers `((get (spec/registry) ~spec) x))
      (if (instance? HyExpression spec)
        (do (setv var (gensym))
            (+= setter `(~var ~spec))
            (.append fetchers `(~var x)))
        (.append fetchers `(~spec x)))))
  `(do (import [minotauro.spec [spec/registry]])
       ~setter
       (fn [x] (and ~@fetchers))))

;;
(defmacro spec/or [&rest specs]
  (setv fetchers [])
  (setv setter '(setv))
  (for [spec specs]
    (setv spec (macroexpand spec))
    (if (keyword? spec)
      (.append fetchers `((get (spec/registry) ~spec) x))
      (if (instance? HyExpression spec)
        (do (setv var (gensym))
            (+= setter `(~var ~spec))
            (.append fetchers `(~var x)))
        (.append fetchers `(~spec x)))))
  `(do (import [minotauro.spec [spec/registry]])
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
                     `((get (spec/registry) ~keys) x)
                     (if (instance? HyExpression keys)
                       (do (setv var (gensym))
                           (+= setter `(~var ~keys))
                           `(~var x))
                       `(~keys x))))
  (setv vfetcher (if (keyword? vals)
                     `((get (spec/registry) ~vals) x)
                     (if (instance? HyExpression vals)
                       (do (setv var (gensym))
                           (+= setter `(~var ~vals))
                           `(~var x))
                       `(~vals x))))
  `(do (import [minotauro.spec [spec/registry]])
       ~setter
       (fn [dict-x] (and (not (some zero? (lfor x (.keys dict-x) ~kfetcher)))
                         (not (some zero? (lfor x (.values dict-x) ~vfetcher)))))))

;;
(defmacro spec/coll-of [spec]
  (setv spec (macroexpand spec))
  (setv setter '(setv))
  (setv fetcher (if (keyword? spec
                     `((get (spec/registry) ~spec) x)
                     (if (instance? HyExpression spec)
                       (do (setv var (gensym))
                           (+= setter `(~var ~spec))
                           `(~var x))
                       `(~spec x)))))
  `(do (import [minotauro.spec [spec/registry]])
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
                  `(get (spec/registry) ~spec)
                  (if (instance? HyExpression spec)
                    (do (setv var (gensym))
                        (+= setter `(~var ~spec))
                        var)
                    spec)))
  `(do (import [minotauro.spec [spec/registry]])
       ~setter
       (~fetcher ~data)))

;;
;; (defmacro spec/assert [spec data])

;;
(defmacro spec/conform [spec data]
  (setv spec (macroexpand spec))
  (setv setter '(setv))
  (setv fetcher (if (keyword? spec)
                  `(get (spec/registry) ~spec)
                  (if (instance? HyExpression spec)
                    (do (setv var (gensym))
                        (+= setter `(~var ~spec))
                        var)
                    spec)))
  `(do (import [minotauro.spec [spec/registry]])
       ~setter
       (if (~fetcher ~data)
         ~data
         (assert False "spec/conform: Data does not conform to spec."))))

;;
;;(defmacro spec/explain [spec data])

;;
(defmacro spec/regex? [regex]
  `(do (import re)
       (let [regexp (re.compile ~regex)]
         (fn [x] (if (.search regexp x) True False)))))


;-----PREDICATES------
