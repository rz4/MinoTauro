(import [lisp [*]])
(require [lisp [*]])
(import torch)

; list|do - do varient which returns an s-expression containing all returned
; values from the evaluated expression in the do.
(defmu list|do (l &rest list_)
  (if (> (len list_) 0)
    `(cons (do  ~l) (list|do ~@list_))
    `(cons (do  ~l) '())))

; list|->
(defun list|-> (l1 l2)
  (if (= (len l1) (len l2)) '()
    (raise (ValueError "list|->:requires equal number of elements in arguments l1 and l2.")))
  (if (= (len l1) 0) `()
    (cons `(-> ~(car l1) ~(car l2)) (list|-> (cdr l1) (cdr l2)))))

; list|->>
(defun list|->> (l1 l2)
  (if (= (len l1) (len l2)) '()
    (raise (ValueError "list|->:requires equal number of elements in arguments l1 and l2.")))
  (if (= (len l1) 0) `()
    (cons `(->> ~(car l1) ~(car l2)) (list|-> (cdr l1) (cdr l2)))))
