;;; thread.hy
;; Updated: 11/13/19
;; File defines threading macros for HyTorch environment.
;;
;; To use macros, import using:
;; (require [hytorch.thread [*]])

; Imports
(import hy)

;; Overloaded thread first macro:
;; Macroexpands all forms including head prior to threading.
;;
(defmacro -> [head &rest forms]
  (setv ret (macroexpand head))
  (for [form forms]
    (setv ret (if (isinstance form HyExpression)
                  (macroexpand `(~(first form) ~ret ~@(rest form)))
                  (macroexpand `(~form ~ret)))))
  ret)

;; Overloaded thread last macro:
;; Macroexpands all forms including head prior to threading.
;;
(defmacro ->> [head &rest forms]
    (setv ret (macroexpand head))
    (for [form forms]
      (setv ret (if (isinstance form HyExpression)
                    (macroexpand `(~(first form) ~@(rest form) ~ret))
                    (macroexpand `(~form ~ret)))))
    ret)

;; Broadcast-thread first macro:
;; Variation of the thread first macro which threads listed forms as the first
;; n arguments in the next form.
;; Example:
;;
;; (*-> [x y] +) : (+ x y)
;;
;; When a form precedes a list of forms broadcast the preceding form to each
;; form in the list.
;; Example:
;;
;; (*-> x [inc decr]) : [(inc x) (decr x)]
;;
(defmacro *-> [head &rest forms]
  (setv ret (macroexpand head))
  (for [form forms]
    (setv ret (macroexpand
                (if (instance? HyExpression form)
                  (if (instance? HyList ret)
                    (do (setv accum []) (for [r ret] (.append accum (macroexpand r)))
                        `(~(first form) ~@accum ~@(rest form)))
                    `(~(first form) ~ret ~@(rest form)))
                  (if (instance? HyList form)
                    (do (setv accum '[])
                        (for [n form]
                          (.append accum (macroexpand `(*-> ~ret ~n))))
                        (HyList accum))
                    (if (instance? HyList ret)
                      (do (setv accum []) (for [r ret] (.append accum (macroexpand r)))
                        `(~form ~@accum))
                      `(~form ~ret)))))))
  ret)

;; Broadcast-thread last macro:
;; Variation of the thread last macro which threads listed forms as
;; the last n arguments in the next form.
;;
(defmacro *->> [head &rest forms]
  (setv ret (macroexpand head))
  (for [form forms]
    (setv ret (macroexpand
                (if (instance? HyExpression form)
                  (if (instance? HyList ret)
                    (do (setv accum []) (for [r ret] (.append accum (macroexpand r)))
                        `(~(first form) ~@(rest form) ~@accum))
                    `(~(first form) ~@(rest form) ~ret))
                  (if (instance? HyList form)
                    (do (setv accum '[])
                        (for [n form]
                          (.append accum (macroexpand `(*->> ~ret ~n))))
                        (HyList accum))
                    (if (instance? HyList form)
                      (do (setv accum []) (for [r ret] (.append accum (macroexpand r)))
                        `(~form ~@accum))
                      `(~form ~ret)))))))
  ret)

;; Inline-thread first macro:
;; Variation of the thread first macro which threads listed forms in parallel.
;; Example:
;;
;; (|-> [x y] [inc decr]) : [(incr x) (decr y)]
;;
;; If a list of forms precedes a single form, thread form along each branch.
;; Example:
;;
;; (|-> [x y] (+ 1)) : [(+ x 1) (+ y 1)]

(defmacro |-> [head &rest forms]
  (setv ret (macroexpand head))
  (for [form forms]
    (setv ret (macroexpand
                (cond [(instance? HyList form)
                       (if (instance? HyList ret)
                           (do (if (!= (len form) (len ret))
                                   (raise (ValueError "|->: Dimension Mismatch")))
                               (setv accum '[])
                               (for [i (range (len form))]
                                    (.append accum (macroexpand `(|-> ~(get ret i) ~(get form i)))))
                               accum)
                           (do (setv accum '[])
                               (for [n form] (.append accum (macroexpand `(|-> ~ret ~n))))
                               accum))]
                      [(instance? HyExpression form)
                       (if (instance? HyList ret)
                           (do (setv accum '[])
                               (for [r ret] (.append accum `(~(first form) ~r ~@(rest form))))
                               accum)
                           `(~(first form) ~ret ~@(rest form)))]
                      [True (if (instance? HyList ret)
                                (do (setv accum '[])
                                    (for [r ret] (.append accum `(~form ~r)))
                                    accum)
                                `(~form ~ret))]))))
  ret)

;; Inline-thread last macro:
;; Variation of the thread last macro which threads listed forms in parallel.
;;
(defmacro |->> [head &rest forms]
  (setv ret (macroexpand head))
  (for [form forms]
    (setv ret (macroexpand
                (cond [(instance? HyList form)
                       (if (instance? HyList ret)
                           (do (if (!= (len form) (len ret))
                                   (raise (ValueError "|->: Dimension Mismatch")))
                               (setv accum '[])
                               (for [i (range (len form))]
                                    (.append accum (macroexpand `(|->> ~(get ret i) ~(get form i)))))
                               accum)
                           (do (setv accum '[])
                               (for [n form] (.append accum (macroexpand `(|->> ~ret ~n))))
                               accum))]
                      [(instance? HyExpression form)
                       (if (instance? HyList ret)
                           (do (setv accum '[])
                               (for [r ret] (.append accum `(~(first form) ~@(rest form) ~r)))
                               accum)
                           `(~(first form) ~@(rest form) ~ret))]
                      [True (if (instance? HyList ret)
                                (do (setv accum '[])
                                    (for [r ret] (.append accum `(~form ~r)))
                                    accum)
                                `(~form ~ret))]))))
  ret)

;; Setv-thread first macro:
;; Variation of the thread first macro which stores the result at each form thread.
;; Used when its more effcient to pass pointer of the evaluated form to next form operation instead
;; of unevaluated form. Gensym used for variable name to prvent namespace collisions.
;; Example:
;;
;; (=-> (+ x 1) incr) : (do (setv g!123 (+ x 1)) (setv g!123 (incr g!123)) g!123)

(defmacro =-> [head &rest forms]
  (setv ret '(do))
  (setv var (gensym))
  (.append ret `(setv ~var ~(macroexpand head)))
  (for [form forms]
    (setv expr `(setv ~var))
    (.append expr (if (isinstance form HyExpression)
                      (macroexpand `(~(first form) ~var ~@(rest form)))
                      (macroexpand `(~form ~var))))
    (.append ret expr))
  (.append ret var)
  ret)

;; Setv-thread last macro:
;; Variation of the thread last macro which stores the result at each form thread.
;;
(defmacro =->> [&rest forms]
  (setv ret '(do))
  (setv var (gensym))
  (.append ret `(setv ~var ~(macroexpand head)))
  (for [form forms]
    (setv expr `(setv ~var))
    (.append expr (if (isinstance form HyExpression)
                      (macroexpand `(~(first form) ~@(rest form) ~var))
                      (macroexpand `(~form ~var))))
    (.append ret expr))
  (.append ret var)
  ret)

;; Conditional-thread first macro:
;; Variation of the thread first macro which operates as cond-> in Clojure.
;; Example:
;;
;; (cond-> x True incr (even? 2) incr (odd? 2) decr) :
;; (if True (if (even? 2) (if (odd? 2) (decr (incr (incr x))) (incr (incr x))) (incr x)) x)

(defmacro cond-> [head &rest args]
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

;; Conditional-thread last macro:
;; Variation of the thread last macro which operates as cond-> in Clojure.
;;
(defmacro cond->> [head &rest args]
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
