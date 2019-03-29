(import [.lisp [*]])

; |do - do varient which returns an s-expression containing all returned
; values from the evaluated expression in the do.
(defmacro |do [l &rest list_]
  (if (> (len list_) 0)
    `(cons (do  ~l) (list|do ~@list_))
    `(cons (do  ~l) '())))

; Head broadcast-threading
(defmacro *-> [head &rest args]
  (setv ret head)
  (for [node args]
    (setv ret (if (isinstance node HyExpression)
                  (if (isinstance ret HyList)
                    (do (setv temp []) (for [r ret] (.append temp (macroexpand r)))
                        `(~(first node) ~@temp ~@(rest node)))
                    `(~(first node) ~ret ~@(rest node)))
                  (if (isinstance node HyList)
                    (do (setv temp '[])
                        (for [n node]
                          (.append temp (macroexpand `(*-> ~ret ~n))))
                        (HyList temp))
                    (if (isinstance ret HyList)
                      (do (setv temp []) (for [r ret] (.append temp (macroexpand r)))
                        `(~node ~@temp))
                      `(~node ~ret))))))
  ret)

; Tail broadcast-threading
(defmacro *->> [head &rest args]
  (setv ret head)
  (for [node args]
    (setv ret (if (isinstance node HyExpression)
                  (if (isinstance ret HyList)
                    (do (setv temp []) (for [r ret] (.append temp (macroexpand r)))
                        `(~(first node) ~@(rest node) ~@temp))
                    `(~(first node) ~@(rest node) ~ret))
                  (if (isinstance node HyList)
                    (do (setv temp '[])
                        (for [n node]
                          (.append temp (macroexpand `(*->> ~ret ~n))))
                        (HyList temp))
                    (if (isinstance ret HyList)
                      (do (setv temp []) (for [r ret] (.append temp (macroexpand r)))
                        `(~node ~@temp))
                      `(~node ~ret))))))
  ret)
