"""
spec.hy
Updated: 8/7/20
Module defines a specfication system for the MinoTauro developement environment.
Inspired from clojure's spec, MinoTauro's spec includes many base operations from clojure.spec.alpha.

;-- Design Principles

- Should function soley as a macro-imported system.
- Should provide methods to specify PyTorch Objects.

;-- FUTURES

;--

To use macros, import using:
(require [mino.spec [*]])
"""

;- Macros
(require [hy.contrib.walk [let]])

;- Imports
(import hy [hy.contrib.hy-repr [hy-repr]])

;-----Spec Registry------

;--
(defn _spec/data-registry []
  """ Access Data Specs Registry:

  Returns:

    (dict): namespace mapping of keywords to data predicate specifications.
  """
  (global data-spec-registry)
  (try data-spec-registry
    (except [] (setv data-spec-registry {}) data-spec-registry)))

;--
(defn _spec/fun-registry []
  """ Access Function Spec Registry:

  Returns:

    (dict): namespace mapping of keywords to function predicate specifications.
  """
  (global fun-spec-registry)
  (try fun-spec-registry
    (except [] (setv fun-spec-registry {}) fun-spec-registry)))

;--
(defn _spec/gen-registry []
  """ Access Generator Spec Registry:

  Returns:

   (dict): namespace mapping of keywords to generator functions.
  """
  (global gen-spec-registry)
  (try gen-spec-registry
    (except [] (setv gen-spec-registry {}) gen-spec-registry)))

;--
(defn _spec/conform-registry [&optional [reset False]]
  """ Access Conform Spec Registry:

  Returns:

    (dict): namespace mapping of keywords to valid? results since last reset.

  """
  (global conform-registry)
  (try (if reset
           (do (setv conform-registry {}) conform-registry)
           conform-registry)
    (except [] (setv conform-registry {}) conform-registry)))

;--
(defn _spec/eval [spec data &rest fun-args]
  """ Evaulate Data Given Specification:

  Used to return specification predicate output while adding results to the
  conform-registry. If &rest arguments are provided, assume a functional
  specification check and pass values as function inputs.

  Args:

    spec (HyKeyword):
    data (Object):
    fun-args (&rest Object):

  Returns:

    out (bool):


  """
  (setv out (if (> (len fun-args) 0)
                ((get (_spec/fun-registry) spec) data fun-args)
                ((get (_spec/data-registry) spec) data)))
  (assoc (_spec/conform-registry) (gensym) {"spec" spec
                                            "passed?" out
                                            "value" data})
  out)

;-----Spec Definition------

;--
(defmacro spec/def [&rest args]
  """ Define New Data Specifications Macro:

  Takes in pairs of HyKeywords and predicate functions.
  Predicate functions can be constructed using specification macros provided
  in the spec module or through lambda expressions. Macros will be expanded and
  the output predicate functions are assigned to the HyKeyword.

  Args:

    args (&rest)

  Returns:

    (HyExpression):
  """
  ;- Get keyword/predicate pairs.
  (assert (even? (len args)) "Args must be paired. Found odd number of arguments.")
  (setv args (partition args :n 2))

  ;- Construct all register setters
  (setv registers [])
  (for [(, kw-namespace predicate) args]
    (assert (and (instance? HyKeyword kw-namespace)
                 (or (instance? HySymbol predicate) (instance? HyExpression predicate)))
       (.format "Arg pair must be of HyKeywords and HyExpressions. Found {kw} and {expr}."
         :kw (name (type kw-namespace))
         :expr (name (type predicate))))
    (setv predicate (macroexpand predicate))
    (if (instance? HyKeyword predicate)
      (.append registers `(assoc (_spec/data-registry) ~kw-namespace (get (_spec/data-registry) ~predicate)))
      (.append registers `(assoc (_spec/data-registry) ~kw-namespace ~predicate))))

  ;- Returned Expression
  `(do (import [mino.spec [_spec/data-registry]])
       ~@registers))

;--
(defmacro spec/defun [kw-namespace args returns]
  """ Define New Function Specification Macro:

  Creates a function test predicate assigned to a HyKeyword. The function test
  predicate is composed of a predicate evaluating the arguments of the function
  and a predicate evaluating the returned value of the function. These are the
  args and returns parameters respectively.

  Args:

  Returns:

  """
  ;- Fetch Specifications
  (setv setter '(setv)
        var-args (gensym)
        var-returns (gensym)
        var-x (gensym)
        var-params (gensym))
  (if (instance? HyKeyword args)
      (+= setter `(~var-args (fn [x] (_spec/eval (quote ~args) x))))
      (+= setter `(~var-args ~args)))
  (if (instance? HyKeyword returns)
      (+= setter `(~var-returns (fn [x] (_spec/eval (quote ~returns) x))))
      (+= setter `(~var-returns ~returns)))

  ;- Returned Expression
  `(do (import [mino.spec [_spec/fun-registry _spec/eval _spec/conform-registry]])
       ~setter
       (assoc (_spec/fun-registry) ~kw-namespace
          (fn [~var-x ~var-params]
            (setv form '(~var-x))
            (for [arg ~var-params] (+= form [arg]))
            (setv valid-args? (~var-args ~var-params))
            (assoc (_spec/conform-registry) (gensym) {"spec" "Arguments"
                                                      "passed?" valid-args?
                                                      "value" ""})
            (setv out (try (eval form) (except [] (return False)))
                  valid-returns? (~var-returns out))
            (assoc (_spec/conform-registry) (gensym) {"spec" "Returned"
                                                      "passed?" valid-returns?
                                                      "value" ""})
            (and valid-args? valid-returns?)))))

;--
(defmacro spec/defgen [kw-namespace args &rest body]
  """ Define New Generator For Data Specification Macro:

  Args:

  Returns:
  """
  ;- Assert Checks
  (assert (instance? HyKeyword kw-namespace) "Arg 1 must be a HyKeyword.")
  (assert (instance? HyList args)
    (.format "Arg 2 must be HyList. Found {t}" :t (name (type require-components))))
  (assert (every? (fn [x] (instance? HySymbol x)) args)
    "Arg 2 must be HyList of HySymbols.")
  (assert (> (len body) 0)
    "Body must be defined.")

  ;- Returned Expression
  `(do (import [mino.spec [_spec/gen-registry]])
       (assoc (_spec/gen-registry) ~kw-namespace (fn ~args ~@body))))

;-----Spec Construction-----

;--
(defmacro spec/nand [&rest specs]
  """NAND Predicate Composition Macro:
  Constructs a NAND logic operator predicate out of argument specifications.
  Can take in any number of legal specifications.

  Args:

  Returns:
  """
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
  `(do (import [mino.spec [_spec/eval]])
       ~setter
       (fn [~var-x] (not (and ~@fetchers)))))

;--
(defmacro spec/and [&rest specs]
  """AND Predicate Composition Macro:
  Constructs an AND logic operator predicate out of argument specifications.
  Can take in any number of specifications.

  Args:

  Returns:
  """
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
  `(do (import [mino.spec [_spec/eval]])
       ~setter
       (fn [~var-x] (and ~@fetchers))))

;--
(defmacro spec/or [&rest specs]
  """Or Operator Predicate Composition Macro:
  Constructs a Or operation predicate out of specification predicates in arguments.

  Can take in any number of predicates or specification HyKeywords.
  """
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
  `(do (import [mino.spec [_spec/eval]])
       ~setter
       (fn [~var-x] (| ~@fetchers))))

;--
(defmacro spec/dict-of [keys vals]
  """Dictionary-Of Predicate Composition Macro:
  Constructs a predicate to check if data is a dictionary of specifications
  defined for arguments keys and vals.
  """
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
  `(do (import [mino.spec [_spec/eval]])
       ~setter
       (fn [~var] (and (not (some zero? (lfor ~var-x (.keys ~var) ~kfetcher)))
                       (not (some zero? (lfor ~var-x (.values ~var) ~vfetcher)))))))

;-- Collection-of Operator
(defmacro spec/coll-of [spec]
  """Collection-Of Predicate Composition Macro:
  Constructs a predicate to check if data is a collection of specifications
  defined for argument spec.
  """
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
  `(do (import [mino.spec [_spec/eval]])
       ~setter
       (fn [~var] (not (some zero? (lfor ~var-x ~var ~fetcher))))))

;-- Contains Keys Operator
(defmacro spec/keys [&rest args]
  """Has-Keys Predicate Composition Macro:
  Constructs a predicate to check if data has contains key with value of specifications
  as defined by the args.

  Args are partitioned in groups of two so defining key specification takes the form:
  key1 spec1 key2 spec2 ... keyN specN
  """
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
  `(do (import [mino.spec [_spec/eval]])
       ~setter
       (fn [~var-x]
         (try (setv ~var-env (. ~var-x __dict__))
           (except [] (return False)))
         (and ~@fetchers))))

;------REGEX Spec Construction------
(defmacro spec/cat [&rest specs]
  (setv fetchers []
        setter '(setv)
        var-x (gensym))
  (for [(, i spec) (enumerate specs)]
    (setv spec (macroexpand spec))
    (if (keyword? spec)
      (.append fetchers `(_spec/eval (quote ~spec) (get ~var-x ~i)))
      (if (instance? HyExpression spec)
        (do (setv var (gensym))
            (+= setter `(~var ~spec))
            (.append fetchers `(~var (get ~var-x ~i))))
        (.append fetchers `(~spec (get ~var-x ~i))))))
  (setv nb-specs (len fetchers))
  `(do (import [mino.spec [_spec/eval]])
       ~setter
       (fn [~var-x] (and (iterable? ~var-x) (= (len ~var-x) ~nb-specs) ~@fetchers))))

;------PyTorch Dependent Spec------

;-- Define PyTorch Sub-Module Specifications
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
  `(do (import [mino.spec [_spec/eval]])
       ~setter
       (fn [~var-x]
         (when (not (instance? nn.Module ~var-x)) (return False))
         (try (setv ~var-env (get (. ~var-x __dict__) "_modules"))
           (except [] (return False)))
         (and ~@fetchers))))

;-- Define PyTorch Sub-Parameters Specifications
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
  `(do (import [mino.spec [_spec/eval]])
       ~setter
       (fn [~var-x]
         (when (not (instance? nn.Module ~var-x)) (return False))
         (try (setv ~var-env (get (. ~var-x __dict__) "_parameters"))
           (except [] (return False)))
         (and ~@fetchers))))


;-----Spec Operators-----

;-- Check if data is valid according to spec
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
  `(do (import [mino.spec [_spec/eval]])
       ~setter
       ~fetcher))

;-- Pass data if valid according to spec
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
  `(do (import [mino.spec [_spec/eval _spec/conform-registry]])
       ~setter
       (_spec/conform-registry :reset True)
       (assert ~fetcher
          (.join "\n" (+ ["Data does not conform Spec.\n\nTraceback Along Registered Specs:\n"]
                         (lfor (, k v) (.items (_spec/conform-registry))
                           (.format "{p} :{s}, Found: {v}" :s (name (get v "spec"))
                                                           :p (if (get v "passed?")
                                                                  "Passed"
                                                                  "Failed")
                                                           :v (get v "value"))))))
       ~data))

;-- Pass data if valid according to spec
(defmacro spec/exercise [spec func &rest data]
  (assert (instance? HyKeyword spec)
    "Arg 1 must be a HyKeyword corresponding to a registered function Spec.")
  (setv fetcher `(_spec/eval (quote ~spec) ~func ~@data))

  `(do (import [mino.spec [_spec/eval _spec/conform-registry]])
       (_spec/conform-registry :reset True)
       (assert ~fetcher
          (.join "\n" (+ ["Function does not conform Spec.\n\nTraceback Along Registered Specs:\n"]
                         (lfor (, k v) (.items (_spec/conform-registry))
                           (.format "{p} :{s}, Found: {v}" :s (name (get v "spec"))
                                                           :p (if (get v "passed?")
                                                                  "Passed"
                                                                  "Failed")
                                                           :v (get v "value"))))))
       (~func ~@data)))

;-- Return dictionary of valid? results on data according to spec
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
  `(do (import [mino.spec [_spec/eval _spec/conform-registry]])
       (_spec/conform-registry :reset True)
       ~setter
       ~expr
       (_spec/conform-registry)))

;-------Spec Assertions------

;-- Check if Spec Asserts are enabled
(defn _spec/check-asserts? []
  (global check-asserts-flag)
  (try check-asserts-flag (except [] True)))

;-- Specification Assert
(defmacro spec/assert [spec data]
  (setv spec (macroexpand spec)
        setter '(setv))
  (setv fetcher (if (keyword? spec)
                  `(_spec/eval (quote ~spec) ~data)
                  (if (instance? HyExpression spec)
                    (do (setv var (gensym))
                        (+= setter `(~var ~spec))
                        `(~var ~data))
                    `(~spec ~data))))
  `(do (import [mino.spec [_spec/eval _spec/conform-registry _spec/check-asserts?]])
       (when (_spec/check-asserts?)
        (do ~setter
            (_spec/conform-registry :reset True)
            (assert ~fetcher
              (.join "\n" (+ ["Data does not conform Spec.\n\nTraceback Along Registered Specs:\n"]
                             (lfor (, k v) (.items (_spec/conform-registry))
                               (.format "{p} :{s}, Found: {v}" :s (name (get v "spec"))
                                                               :p (if (get v "passed?")
                                                                      "Passed"
                                                                      "Failed")
                                                               :v (get v "value"))))))))))

;-- Enable or Disable Spec Asserts
(defmacro spec/check-asserts [flag]
  `(do (global check-asserts-flag)
       (setv check-asserts-flag ~flag)))

;-----Spec Search & Replace-----

;--
(defmacro spec/search [spec data]
  (macroexpand
    `(do (require [mino.spec [spec/valid?]]
                  [hy.contrib.walk [let]])
         (import [hy.contrib.walk [prewalk]])
         (let [matches []]
           (prewalk (fn [x] (when (spec/valid? ~spec x) (.append matches x)) x) (quote ~data))
           (defn _matches [] (for [match matches] (yield match)))
           (_matches)))))

;--
(defmacro spec/replace [spec data replacement]
  (macroexpand
    `(do (require [mino.spec [spec/valid?]]
                  [hy.contrib.walk [let]])
         (import [hy.contrib.walk [postwalk]])
         (let [matches []]
           (postwalk (fn [x] (if (spec/valid? ~spec x) (quote ~replacement) x)) (quote ~data))))))

;-----Specification Generation-----

;--
(defmacro spec/gen [spec &rest args]
  (setv var-env-gen (gensym)
        var-env-data (gensym)
        var (gensym))
  (setv conformed (macroexpand `(spec/conform ~spec ~var)))
  `(do (import [mino.spec [_spec/gen-registry _spec/data-registry]])
       (setv ~var-env-gen (_spec/gen-registry)
             ~var-env-data (_spec/data-registry))
       (assert (in ~spec ~var-env-gen) "Generator for Spec is not defined.")
       (if (in ~spec ~var-env-data)
         (do (setv ~var ((get (_spec/gen-registry) ~spec) ~@args)) ~conformed)
         ((get (_spec/gen-registry) ~spec) ~@args))))
