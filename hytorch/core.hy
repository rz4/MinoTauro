;;; core.hy
;;

; Imports
(import [hy.models [HyExpression HyList HySymbol]])
(import [hy.contrib.walk [walk prewalk]])
(require [hytorch.thread [*]])

;; |gensym : generates list of HySymbols for list
(defn |gensym [exprs namespace]
  (HyList (lfor i (range (len exprs))
            (HySymbol (+ namespace (str i))))))

;; \setv : sets list of expressions to list of symbols
(defmacro |setv [symbs exprs]
  (if (!= (len `(~@symbs)) (len `(~@exprs)))
    (raise (ValueError "|setv: Length of Symbols and Expressions must match.")))
  `(|-> ~exprs ~symbs quote unquote-splice setv quasiquote eval))

;; print-lisp - pretty Lisp print on single line
(defn walk-print [x]
    (cond [(= HyExpression (type x)) (print-lisp x :end "")]
          [(= HyList (type x)) (print-lisp x :end "")]
          [True (print x :end " ")])
    x)

(defn print-lisp [expr &optional [end "\n"]]
    (cond [(= HyExpression (type expr)) (print "(" :end "")]
          [(= tuple (type expr)) (print "(" :end "")]
          [(= HyList (type expr)) (print "[" :end "")]
          [(= list (type expr)) (print "[" :end "")])
    (walk walk-print identity expr)
    (if (> (len expr) 0) (print "\b" :end ""))
    (cond [(= HyExpression (type expr)) (print ") " :end end)]
          [(= tuple (type expr)) (print ") " :end end)]
          [(= HyList (type expr)) (print "] " :end end)]
          [(= list (type expr)) (print "] " :end end)]))
