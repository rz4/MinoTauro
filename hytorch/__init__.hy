; Imports
(import hy)

; Macros
; some

;-----MODULES------

;; Module Definition
(defmacro defmodule [module-name components forward &rest funcs]

  ; Generate arg expr
  (setv args (lfor c components [c None]))

  ; Generate init-body
  (setv init-body '(setv))
  (for [c components] (+= init-body `((. self ~c) ~c)))

  ; Generate dispatcher
  (setv dispatcher '(setv))
  (for [c components] (+= dispatcher `(~c (if (none? ~c) (. self ~c) ~c))))

  ; Rapture
  (setv forward (macroexpand forward))

  `(do (import [torch.nn [Module]])
       (defclass ~module-name [Module]
         (defn __init__ [self &optional ~@args]
           (.__init__ (super ~module-name self))
           ~init-body)
         (defn forward [self &optional ~@args]
           ~dispatcher
           ~forward)
         ~@funcs)))

;-----OTHER------

;; Macro for mulitdimensional indexing of Numpy, Pandas, and PyTorch arrays:
;; Similar to native python multi-array indexing.
;; Example:
;;
;; (get x [:] [:] [1]) is same as x[:,:,1]
(defmacro geta [x &rest args]
  (setv slices [])
  (setv xx x)
  (for [_ args]
    (setv s '() i 0 t 0)
    (for [j _]
      (cond [(and (= ': j) (= 0 t)) (do (.append s i) (setv t 1 i `(len ~xx)))]
            [(and (= ': j) (= 1 t)) (do (.append s i) (setv i 1))]
            [True (setv i j)]))
    (.append s i)
    (setv xx `(get ~xx 0))
    (if (< 1 (len s)) (.append slices `(slice ~@s)) (.append slices (get s 0))))
  `(get ~x (, ~@slices)))
