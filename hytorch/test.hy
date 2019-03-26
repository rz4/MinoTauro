; Testing NN training

; Imports
(import [hytorch [*]])
(require [hytorch [*]])

; Checking for available cuda device
(setv device (torch.device (if (.is_available torch.cuda) "cuda:0" "cpu")))

; Load inputs
(setv x-defs '((tensor device torch.float 10 10)))
(setv x-leafs (eval `(list|do ~@x-defs)))
(setv x-inits '((-> torch.normal)))
(setv x (init-tensors x-leafs x-inits))

(setv y-defs '((tensor device torch.float 10 1)))
(setv y-leafs (eval `(list|do ~@y-defs)))
(setv y-inits '((-> torch.normal)))
(setv y (init-tensors y-leafs y-inits))

; Define Weight Tensors
(setv nn-w-defs '((tensor device torch.float 10 10)
                  (tensor device torch.float 10 1)))
(setv nn-w-leafs (eval `(list|do ~@nn-w-defs)))
(setv nn-w-inits '((-> torch.normal)
                   (-> torch.normal)))
(setv nn-ws (init-tensors nn-w-leafs nn-w-inits))

; Define Network Operations
(setv nn-ops '(-> t0 (torch.mm t1) torch.sigmoid (torch.mm t2) torch.sigmoid))

; Run Forward Prop
(setv out (forward nn-ops (+ x nn-w-leafs)))

; Define Loss
(setv loss-def (torch.nn.MSELoss :reduction "sum"))

; Define Optimizer
(setv optim (torch.optim.Adam (list nn-w-leafs) :lr 1e-4))

; Run Back Prop
(setv loss (loss-def out (get y 0)))
(.backward loss)
(print (. (get nn-w-leafs 0) grad))
(.zero_grad optim)
(.step optim)
