;;;; Lisp-ier Wrappers for Hy
;; The following macros and are used to provide a more basic Lisp feel to
;; Hy's implementation of Lisp-style Python.
;; This is prodominantly a personal aestheic choice for my own productivity
;; as well as to facilitate some meta programming machine learning experiments.

; Imports
(import hy)

;;; Basic Lisp Primitive Wrappers
;; There are a couple of annoyances I have when working with Hy. For starters,
;; there are a number of functions which require python list-typed parameters.
;; This hardcoded type syntax, while providing measures against improper usage,
;; reduces the meta-expressibility of the langauge. The second minor
;; annoyance, is the lack of car, cdr, and cons which are common operators in
;; all other Lisp dialects for manipulation of S-expressions. I've added these
;; as to accomadate more traditional Lisp style conventions in my own work.

; S-expression defn
(defmacro defun [nme args &rest body]
  (setv params `(~nme ~(list args) ~@body))
  `(defn ~@params))

; S-expression defmacro
(defmacro defmu [nme args &rest body]
  (setv params `(~nme ~(list args) ~@body))
  `(defmacro ~@params))

; S-expression cond
(defmu condi (case &rest cases)
  (if (> (len cases) 0)
    `(if ~@case (condi ~@cases))
    `(if ~@case '())))

; car
(defun car (x) (first x))

; cdr
(defun cdr (x)
  (hy.models.HyExpression (list (rest x))))

; atom?
(defun atom? (x)
  (if (= (type x) hy.models.HyExpression) False True))

; cons
(defun cons (x y)
  (if (atom? y)
    (raise (TypeError "cons-> Second Argument Must Be List."))
    (hy.models.HyExpression (list `(~x ~@y)))))

;;; Hy Object Manipulators
;; These set expressions are used to more easily recognize
;; and manipluate Hy objects. This is used for meta-programming
;; purposes in differenitating between native Python and Hy Lisp.

; symb - makes new hy symbol objects from string
(defun symb (x)
  (if (= (type x) str)
    (hy.models.HySymbol x)
    (raise (TypeError "symb-> Argument Must Be String"))))

; symb?
(defun symb? (x)
  (if (= (type x) hy.models.HySymbol) True False))

; symblist?
(defun symb-list? (x)
  (if (symb? (car x))
    (if (> (- (len x) 1) 0)
      (symb-list? (cdr x))
      True)
    False))

; hy? - Check if x is a Hy object
(defun hy? (x)
  (if (= (cut (name (type x)) 0 2) "Hy") True False))


;;; I/O Macros and Function
;; These set of expressions are used for various
;; input/output purposes including ways of printing
;; Hy expressions in a more legible manner.

; printlisp - pretty Lisp print on single line
(defun printlisp (x &optional [flag 0])
  (if (or (= flag 0) (= flag 1))
    (print "(" :end "") None)
  (if (not (atom? (car x)))
    (printlisp (car x) 1)
    (print (car x) :end ""))
  (if (= '() (cdr x))
    (print ")" :end "")
    (do (print " " :end "") (printlisp (cdr x) 2)))
  (if (= flag 0) (print "")))
