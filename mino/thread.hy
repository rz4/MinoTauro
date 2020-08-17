"""
thread.hy
Updated: 1/4/2020
File defines threading macros for Minotauro development environment.

;--

To use macros, import using:
(require [mino.thread [*]])
"""

;- Imports
(import hy)

;--
(defmacro -> [head &rest forms]
  """Overloaded thread-first macro:

  Macroexpands all forms including head prior to threading.
  Forms will be threaded into the first argument slot.

  Args:

  head (HySymbol or HyExpression): starting element for threading
  forms (&rest HySymbols or HyExpressions): subsequent elements in thread

  Returns:

  ret (HyExpression): Expanded code with threaded expressions

  """
  (setv ret (macroexpand head))
  (for [form forms]
    (setv ret (if (isinstance form HyExpression)
                  (macroexpand `(~(first form) ~ret ~@(rest form)))
                  (macroexpand `(~form ~ret)))))
  ret)

(defmacro ->> [head &rest forms]
  """Overloaded thread-last macro:

  Macroexpands all forms including head prior to threading.
  Forms will be threaded into the last argument slot.

  Args:

  head (HySymbol or HyExpression): starting element for threading
  forms (&rest HySymbols or HyExpressions): subsequent elements in thread

  Returns:

  ret (HyExpression): Expanded code with threaded expressions
  """
  (setv ret (macroexpand head))
  (for [form forms]
    (setv ret (if (isinstance form HyExpression)
                  (macroexpand `(~(first form) ~@(rest form) ~ret))
                  (macroexpand `(~form ~ret)))))
  ret)

(defmacro *-> [head &rest forms]
  """ Broadcast-thread first macro:

  Variation of the thread first macro which threads listed forms as the first
  n arguments in the next form. When a form precedes a list of forms broadcast the preceding form to each form in the list.

  Args:

  head (HySymbol or HyExpression): starting element for threading
  forms (&rest HySymbols or HyExpressions): subsequent elements in thread

  Returns:

  ret (HyExpression): Expanded code with threaded expressions

  Example:

  (*-> [x y] +)

  ; Returns
  (+ x y)

  (*-> x [inc decr])

  ; Returns
  [(inc x) (decr x)]
  """
  (setv ret (macroexpand head))
  (for [form forms]
    (setv ret (macroexpand
                (if (instance? HyExpression form)
                  (if (instance? HyList ret)
                    (do (setv accum []) (for [r ret] (+= accum (macroexpand `(~r))))
                        `(~(first form) ~@accum ~@(rest form)))
                    `(~(first form) ~ret ~@(rest form)))
                  (if (instance? HyList form)
                    (do (setv accum '[])
                        (for [n form]
                          (+= accum (macroexpand `((*-> ~ret ~n)))))
                        (HyList accum))
                    (if (instance? HyList ret)
                      (do (setv accum []) (for [r ret] (+= accum (macroexpand `(~r))))
                        `(~form ~@accum))
                      `(~form ~ret)))))))
  ret)

(defmacro *->> [head &rest forms]
  """Broadcast-thread last macro:

  Variation of the thread last macro which threads listed forms as
  the last n arguments in the next form.

  Args:

  head (HySymbol or HyExpression): starting element for threading
  forms (&rest HySymbols or HyExpressions): subsequent elements in thread

  Returns:

  ret (HyExpression): Expanded code with threaded expressions
  """
  (setv ret (macroexpand head))
  (for [form forms]
    (setv ret (macroexpand
                (if (instance? HyExpression form)
                  (if (instance? HyList ret)
                    (do (setv accum []) (for [r ret] (+= accum (macroexpand `(~r))))
                        `(~(first form) ~@(rest form) ~@accum))
                    `(~(first form) ~@(rest form) ~ret))
                  (if (instance? HyList form)
                    (do (setv accum '[])
                        (for [n form]
                          (+= accum (macroexpand `((*->> ~ret ~n)))))
                        (HyList accum))
                    (if (instance? HyList form)
                      (do (setv accum []) (for [r ret] (+= accum (macroexpand `(~r))))
                        `(~form ~@accum))
                      `(~form ~ret)))))))
  ret)

(defmacro |-> [head &rest forms]
  """Inline-thread first macro:

  Variation of the thread first macro which threads listed forms in parallel.
  Example: (|-> [x y] [inc decr])

  Returns: [(incr x) (decr y)]

  If a list of forms precedes a single form, thread form along each branch.
  Example: (|-> [x y] (+ 1))

  Returns: [(+ x 1) (+ y 1)]
  """
  (setv ret (macroexpand head))
  (for [form forms]
    (setv ret (macroexpand
                (cond [(instance? HyList form)
                       (if (instance? HyList ret)
                           (do (if (!= (len form) (len ret))
                                   (raise (ValueError "|->: Dimension Mismatch")))
                               (setv accum '[])
                               (for [i (range (len form))]
                                    (+= accum (macroexpand `((|-> ~(get ret i) ~(get form i))))))
                               accum)
                           (do (setv accum '[])
                               (for [n form] (+= accum (macroexpand `((|-> ~ret ~n)))))
                               accum))]
                      [(instance? HyExpression form)
                       (if (instance? HyList ret)
                           (do (setv accum '[])
                               (for [r ret] (+= accum `((~(first form) ~r ~@(rest form)))))
                               accum)
                           `(~(first form) ~ret ~@(rest form)))]
                      [True (if (instance? HyList ret)
                                (do (setv accum '[])
                                    (for [r ret] (+= accum `((~form ~r))))
                                    accum)
                                `(~form ~ret))]))))
  ret)

(defmacro |->> [head &rest forms]
  """Inline-thread last macro:

  Variation of the thread last macro which threads listed forms in parallel.
  """
  (setv ret (macroexpand head))
  (for [form forms]
    (setv ret (macroexpand
                (cond [(instance? HyList form)
                       (if (instance? HyList ret)
                           (do (if (!= (len form) (len ret))
                                   (raise (ValueError "|->: Dimension Mismatch")))
                               (setv accum '[])
                               (for [i (range (len form))]
                                    (+= accum (macroexpand `((|->> ~(get ret i) ~(get form i))))))
                               accum)
                           (do (setv accum '[])
                               (for [n form] (+= accum (macroexpand `((|->> ~ret ~n)))))
                               accum))]
                      [(instance? HyExpression form)
                       (if (instance? HyList ret)
                           (do (setv accum '[])
                               (for [r ret] (+= accum `((~(first form) ~@(rest form) ~r))))
                               accum)
                           `(~(first form) ~@(rest form) ~ret))]
                      [True (if (instance? HyList ret)
                                (do (setv accum '[])
                                    (for [r ret] (+= accum `((~form ~r))))
                                    accum)
                                `(~form ~ret))]))))
  ret)

(defmacro =-> [head &rest forms]
  """Setv-thread first macro:

  Variation of the thread first macro which stores the result at each form thread.
  Used when its more effcient to pass pointer of the evaluated form to next form operation instead
  of unevaluated form. Gensym used for variable name to prvent namespace collisions.
  Example: (=-> (+ x 1) incr)

  Returns: (do (setv g!123 (+ x 1)) (setv g!123 (incr g!123)) g!123)
  """
  (setv ret '(do))
  (setv var (gensym))
  (+= ret `((setv ~var ~(macroexpand head))))
  (for [form forms]
    (setv expr `(setv ~var))
    (+= expr (if (isinstance form HyExpression)
                (macroexpand `((~(first form) ~var ~@(rest form))))
                (macroexpand `((~form ~var)))))
    (+= ret `(~expr)))
  (+= ret `(~var))
  ret)

(defmacro =->> [&rest forms]
  """Setv-thread last macro:

  Variation of the thread last macro which stores the result at each form thread.
  """
  (setv ret '(do))
  (setv var (gensym))
  (+= ret `((setv ~var ~(macroexpand head))))
  (for [form forms]
    (setv expr `(setv ~var))
    (+= expr (if (isinstance form HyExpression)
                (macroexpand `((~(first form) ~@(rest form) ~var)))
                (macroexpand `((~form ~var)))))
    (+= ret `(~expr)))
  (+= ret `(~var))
  ret)

(defmacro cond-> [head &rest args]
  """Conditional-thread first macro:

  Variation of the thread first macro which operates as cond-> in Clojure.
  Example: (cond-> x True incr (even? 2) incr (odd? 2) decr)

  Returns: (if True (if (even? 2) (if (odd? 2) (decr (incr (incr x))) (incr (incr x))) (incr x)) x)
  """
  (assert (even? (len args)) "cond->: Wrong number of arguments.")
  (setv pairs (partition args :n 2)
        thread (macroexpand head))
  (setv conditions []
        forms [thread])
  (for [p pairs]
    (setv (, clause form) p)
    (setv thread (if (isinstance form HyExpression)
                     (macroexpand `(~(first form) ~thread ~@(rest form)))
                     (macroexpand `(~form ~thread))))
    (.append conditions clause)
    (.append forms thread))

  (setv ret None)
  (for [(, i clause) (enumerate (reversed conditions))]
    (if (none? ret)
      (setv ret `(if ~clause ~(get forms (- -1 i)) ~(get forms (- -2 i))))
      (setv ret `(if ~clause ~ret ~(get forms (- -2 i))))))
  ret)

(defmacro cond->> [head &rest args]
  """Conditional-thread last macro:

  Variation of the thread last macro which operates as cond-> in Clojure.
  """
  (assert (even? (len args)) "cond->: Wrong number of arguments.")
  (setv pairs (partition args :n 2)
        thread (macroexpand head))
  (setv conditions []
        forms [thread])
  (for [p pairs]
    (setv (, clause form) p)
    (setv thread (if (isinstance form HyExpression)
                     (macroexpand `(~(first form) ~@(rest form) ~thread))
                     (macroexpand `(~form ~thread))))
    (.append conditions clause)
    (.append forms thread))

  (setv ret None)
  (for [(, i clause) (enumerate (reversed conditions))]
    (if (none? ret)
      (setv ret `(if ~clause ~(get forms (- -1 i)) ~(get forms (- -2 i))))
      (setv ret `(if ~clause ~ret ~(get forms (- -2 i))))))
  ret)
