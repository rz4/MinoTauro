;;;; Core Pytorch Lisp Bindings in Hy
;; README:
;; This file contains some macros and functions for processing pytorch data objects as Lisp
;; S-expressions.


; Imports
(import [lisp [*]])
(require [lisp [*]])
(import torch)

;;; Macros and functions for general workflow

; tensor - macro to create new leaf tensors with more lisp style syntax
(defmu tensor (device dtype &rest dims)
  `(torch.empty ~@dims :dtype ~dtype :device ~device :requires_grad True))

; init-tensors - function applies list of init-ops to list of unintialized tensors
; and returns new tensors.
(defun init-tensors (tensors init-ops)
  (if (= (len tensors) (len init-ops)) '()
    (raise
      (ValueError "init-tensors->requires equal number of tensors and inits.")))
  (setv tensors- '())
  (for [x (range (len tensors))]
    (eval (cons 'setv `(~(symb (+ "t" (str x))) ~(quote (get tensors x)))))
    (setv tensors- (cons `(-> ~(symb (+ "t" (str x))) ~(get init-ops x)) tensors-)))
  (eval (cons 'list|do tensors-)))

; forward - function runs forward propagation using the network operations defined in nn-ops.
; tensors are defined in nn-ops as t'n' where n is the index of the tensor in nn-ws.
(defun forward (nn-ops nn-ws)
  (for [x (range (len nn-ws))]
    (eval (cons 'setv `(~(symb (+ "t" (str x))) ~(quote (get nn-ws x))))))
  (eval nn-ops))
