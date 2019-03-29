; Imports
(import [hy.models [*]])
(import torch)

; forward - function runs forward propagation using the network operations defined in nn-ops.
; tensors are defined in nn-ops as t'n' where n is the index of the tensor in nn-ws.
(defn forward [nn-ops nn-ws &optional [training False]]
  (setv training trianing)
  (for [x (range (len nn-ws))]
    (eval (cons 'setv `(~(HySymbol (+ "t" (str x))) ~(quote (get nn-ws x))))))
  (eval nn-ops))
