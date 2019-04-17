; Imports
(import [hy.models [HyList HySymbol]])
(require [hytorch.thread [*]])

; |gensym : generates list of HySymbols for list
(defn |gensym [exprs namespace]
  (HyList (lfor i (range (len exprs))
            (HySymbol (+ namespace (str i))))))

; \setv : sets list of expressions to list of symbols
(defmacro |setv [symbs exprs]
  (if (!= (len `(~@symbs)) (len `(~@exprs)))
    (raise (ValueError "|setv: Length of Symbols and Expressions must match.")))
  `(|-> ~exprs ~symbs quote unquote-splice setv quasiquote eval))
