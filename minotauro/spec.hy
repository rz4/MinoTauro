;;; spec.hy
;; Updated: 12/12/19
;; File defines specfication system used in within the Minotauro developement environment.
;; Inspired from clojure's spec. Includes many base operations from clojure.spec.alpha.
;;
;; Design Principles:
;;
;; - Spec should function soley as a macro-imported system.
;; - Make sure to use gensym to prevent namespace collisions.
;; - Provide useful debugging messages to accelerate PyTorch model implementation.
;;
;; To use macros, import using:
;; (require [minotauro.spec [*]])

; Imports
(import hy)

; Macros
(require [hy.contrib.walk [let]])

;-----Spec Registry------

;; Returns Specification Registry
(defn spec/registry []
  (global spec-registry)
  (try spec-registry
    (except [] (setv spec-registry {}) spec-registry)))

;; Returns Specification Generator Registry
(defn spec/gen-registry []
  (global gen-spec-registry)
  (try gen-spec-registry
    (except [] (setv gen-spec-registry {}) gen-spec-registry)))

;; Returns Conformed Specification Temporary Registry
(defn _spec/conform-registry [&optional [reset False]]
  (global conform-registry)
  (try (if reset
           (do (setv conform-registry {}) conform-registry)
           conform-registry)
    (except [] (setv conform-registry {}) conform-registry)))

;; Specification Evaluator:
; Fetches specification from registry and evaluates on data.
; Adds evaluation value to conform registry.
(defn _spec/eval [spec data &optional]
  (setv out ((get (spec/registry) spec) data))
  (assoc (_spec/conform-registry) spec out)
  out)

;;-----Spec Definintion------

;; Defines new specfication in registry:
; Can take multiple specification pairs.
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

;;-----Spec Construction-----

;; Nand Operator:
; Can take in any number of predicates or specification keys
(defmacro spec/nand [&rest specs]
  (setv fetchers []
        setter '(setv)
        var-x (gensym))
  (for [spec specs]
    (setv spec (macroexpand spec))
    (if (keyword? spec)
      (.append fetchers `(_spec/eval (quote ~spec) ~var-x))
      (if (instance? HyExpression spec)
        (do (setv var (gensym))
            (+= setter `(~var ~spec))
            (.append fetchers `(~var ~var-x)))
        (.append fetchers `(~spec ~var-x)))))
  `(do (import [minotauro.spec [_spec/eval]])
       ~setter
       (fn [~var-x] (not (and ~@fetchers)))))

;; And Operator:
; Can take in any number of predicates or specification keys
(defmacro spec/and [&rest specs]
  (setv fetchers []
        setter '(setv)
        var-x (gensym))
  (for [spec specs]
    (setv spec (macroexpand spec))
    (if (keyword? spec)
      (.append fetchers `(_spec/eval (quote ~spec) ~var-x))
      (if (instance? HyExpression spec)
        (do (setv var (gensym))
            (+= setter `(~var ~spec))
            (.append fetchers `(~var ~var-x)))
        (.append fetchers `(~spec ~var-x)))))
  `(do (import [minotauro.spec [_spec/eval]])
       ~setter
       (fn [~var-x] (and ~@fetchers))))

;; Or Operator
; Can take in any number of predicates or specification keys
(defmacro spec/or [&rest specs]
  (setv fetchers []
        setter '(setv)
        var-x (gensym))
  (for [spec specs]
    (setv spec (macroexpand spec))
    (if (keyword? spec)
      (.append fetchers `(_spec/eval (quote ~spec) ~var-x))
      (if (instance? HyExpression spec)
        (do (setv var (gensym))
            (+= setter `(~var ~spec))
            (.append fetchers `(~var ~var-x)))
        (.append fetchers `(~spec ~var-x)))))
  `(do (import [minotauro.spec [_spec/eval]])
       ~setter
       (fn [~var-x] (| ~@fetchers))))

;; Dictionary-of keys and vals Operator
(defmacro spec/dict-of [keys vals]
  (setv keys (macroexpand keys)
        vals (macroexpand vals)
        var-x (gensym)
        var (gensym))
  (setv setter '(setv)
        kfetcher (if (keyword? keys)
                     `(_spec/eval (quote ~keys) ~var-x)
                     (if (instance? HyExpression keys)
                       (do (setv var (gensym))
                           (+= setter `(~var ~keys))
                           `(~var ~var-x))
                       `(~keys ~var-x)))
        vfetcher (if (keyword? vals)
                     `(_spec/eval (quote ~vals) ~var-x)
                     (if (instance? HyExpression vals)
                       (do (setv var (gensym))
                           (+= setter `(~var ~vals))
                           `(~var ~var-x))
                       `(~vals ~var-x))))
  `(do (import [minotauro.spec [_spec/eval]])
       ~setter
       (fn [~var] (and (not (some zero? (lfor ~var-x (.keys ~var) ~kfetcher)))
                       (not (some zero? (lfor ~var-x (.values ~var) ~vfetcher)))))))

;; Collection-of Operator
(defmacro spec/coll-of [spec]
  (setv spec (macroexpand spec)
        setter '(setv)
        var-x (gensym)
        var (gensym))
  (setv fetcher (if (keyword? spec
                     `(_spec/eval (quote ~spec) ~var-x)
                     (if (instance? HyExpression spec)
                       (do (setv var (gensym))
                           (+= setter `(~var ~spec))
                           `(~var ~var-x))
                       `(~spec ~var-x)))))
  `(do (import [minotauro.spec [_spec/eval]])
       ~setter
       (fn [~var] (not (some zero? (lfor ~var-x ~var ~fetcher))))))

;; Contains Keys Operator
(defmacro spec/keys [&rest args]
  (setv args (partition args :n 2)
        fetchers []
        setter '(setv)
        var-env (gensym)
        var-x (gensym))
  (for [(, key spec) args]
    (setv spec (macroexpand spec))
    (if (keyword? spec)
      (setv spec-check `(_spec/eval (quote ~spec) (get ~var-env (quote ~key))))
      (if (instance? HyExpression spec)
        (do (setv var (gensym))
            (+= setter `(~var ~spec))
            (setv spec-check `(~var (get ~var-env (quote ~key)))))
        (setv spec-check `(~spec (get ~var-env (quote ~key))))))
    (.append fetchers `(if (in (quote ~key) ~var-env) ~spec-check False)))
  `(do (import [minotauro.spec [_spec/eval]])
       ~setter
       (fn [~var-x]
         (try (setv ~var-env (. ~var-x __dict__))
           (except [] (return False)))
         (and ~@fetchers))))

;;------REGEX Spec Construction------

;;
;(defmacro spec/cat [&rest specs])

;;


;;------PyTorch Dependent Spec------

;; Define PyTorch Sub-Module Specifications
; Returns False if data is not a PyTorch module.
(defmacro spec/modules [&rest args]
  (setv args (partition args :n 2)
        fetchers []
        setter '(setv)
        var-env (gensym)
        var-x (gensym))
  (for [(, key spec) args]
    (setv spec (macroexpand spec))
    (setv key (mangle key))
    (if (keyword? spec)
      (setv spec-check `(_spec/eval (quote ~spec) (get ~var-env (quote ~key))))
      (if (instance? HyExpression spec)
        (do (setv var (gensym))
            (+= setter `(~var ~spec))
            (setv spec-check `(~var (get ~var-env (quote ~key)))))
        (setv spec-check `(~spec (get ~var-env (quote ~key))))))
    (.append fetchers `(if (in (quote ~key) ~var-env) ~spec-check False)))
  `(do (import [minotauro.spec [_spec/eval]])
       ~setter
       (fn [~var-x]
         (when (not (instance? nn.Module x)) (return False))
         (try (setv ~var-env (get (. ~var-x __dict__) "_modules"))
           (except [] (return False)))
         (and ~@fetchers))))

;; Define PyTorch Sub-Parameters Specifications
; Returns False if data is not a PyTorch module.
(defmacro spec/parameters [&rest args]
  (setv args (partition args :n 2)
        fetchers []
        setter '(setv)
        var-env (gensym)
        var-x (gensym))
  (for [(, key spec) args]
    (setv spec (macroexpand spec))
    (setv key (mangle key))
    (if (keyword? spec)
      (setv spec-check `(_spec/eval (quote ~spec) (get ~var-env (quote ~key))))
      (if (instance? HyExpression spec)
        (do (setv var (gensym))
            (+= setter `(~var ~spec))
            (setv spec-check `(~var (get ~var-env (quote ~key)))))
        (setv spec-check `(~spec (get ~var-env (quote ~key))))))
    (.append fetchers `(if (in (quote ~key) ~var-env) ~spec-check False)))
  `(do (import [minotauro.spec [_spec/eval]])
       ~setter
       (fn [~var-x]
         (when (not (instance? nn.Module x)) (return False))
         (try (setv ~var-env (get (. ~var-x __dict__) "_parameters"))
           (except [] (return False)))
         (and ~@fetchers))))


;;---Spec Operators---

;; Check if data is valid according to spec
(defmacro spec/valid? [spec data]
  (setv spec (macroexpand spec)
        setter '(setv))
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
  (setv spec (macroexpand spec)
        setter '(setv))
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
  (setv spec (macroexpand spec)
        setter '(setv))
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

;; Return dictionary of predicate evaluations given spec and data.
;;(defmacro spec/describe [spec data])

;;------Spec Assertions------

;; Specification Assert
;(defmacro spec/assert [])

;; Enable or Disable Spec Asserts
;(defmacro spec/check-asserts [flag])

;; Check if Spec Asserts are enabled
;(defmacro spec/check-asserts? [])

;-----Specification Generation-----

;; Specification Generator
(defmacro spec/gen [spec &rest args]
  (setv var-env (gensym)
        var (gensym))
  (setv conformed (macroexpand `(spec/conform ~spec ~var)))
  `(do (import [minotauro.spec [spec/gen-registry]])
       (setv ~var-env (spec/gen-registry))
       (if (in ~spec ~var-env)
         (do (setv ~var ((get (spec/gen-registry) ~spec) ~@args))
             ~conformed)
         (assert False "spec/gen: Generator not defined."))))
