;;; thread.hy
;;; Collection of threading macros for inline definition of tensor operations

;; Head broadcast-threading
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

;; Tail broadcast-threading
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

;; Head list-threading
(defmacro |-> [head &rest args]
  (setv ret head)
  (for [node args]
    (setv ret (cond [(isinstance node HyList)
                     (if (isinstance ret HyList)
                         (do (if (!= (len node) (len ret))
                                 (raise (ValueError "|->: Dimension Mismatch")))
                             (setv temp '[])
                             (for [i (range (len node))]
                                  (.append temp (macroexpand `(|-> ~(get ret i) ~(get node i)))))
                             temp)
                         (do (setv temp '[])
                             (for [n node] (.append temp (macroexpand `(|-> ~ret ~n))))
                             temp))]
                    [(isinstance node HyExpression)
                     (if (isinstance ret HyList)
                         (do (setv temp '[])
                             (for [r ret] (.append temp `(~(first node) ~r ~@(rest node))))
                             temp)
                         `(~(first node) ~ret ~@(rest node)))]
                    [True (if (isinstance ret HyList)
                              (do (setv temp '[])
                                  (for [r ret] (.append temp `(~node ~r)))
                                  temp)
                              `(~node ~ret))])))
  ret)

;; Tail list-threading
(defmacro |->> [head &rest args]
  (setv ret head)
  (for [node args]
    (setv ret (cond [(isinstance node HyList)
                     (if (isinstance ret HyList)
                         (do (if (!= (len node) (len ret))
                                 (raise (ValueError "|->>: Dimension Mismatch")))
                             (setv temp '[])
                             (for [i (range (len node))]
                                  (.append temp (macroexpand `(|->> ~(get ret i) ~(get node i)))))
                             temp)
                         (do (setv temp '[])
                             (for [n node] (.append temp (macroexpand `(|->> ~ret ~n))))
                             temp))]
                    [(isinstance node HyExpression)
                     (if (isinstance ret HyList)
                         (do (setv temp '[])
                             (for [r ret] (.append temp `(~(first node) ~@(rest node) ~r)))
                             temp)
                         `(~(first node) ~@(rest node) ~ret))]
                    [True (if (isinstance ret HyList)
                              (do (setv temp '[])
                                  (for [r ret] (.append temp `(~node ~r)))
                                  temp)
                              `(~node ~ret))])))
  ret)
