![LOGO](images/logo.png)
# (setv HyTorch (+ Hy PyTorch))
PyTorch Meta-Programming Using the Lisp Dialect Hy

![Current Version](https://img.shields.io/badge/version-0.0.0-red.svg)

Lead Maintainer: [Rafael Zamora-Resendiz](https://github.com/rz4)

**HyTorch** is a Hy (0.16.0) library running Python (3.7) and PyTorch (1.0.1)
for use in rapid low-level development of deep learning (DL) systems as well as
for experiments in DL meta-programming.

## Motivation
The dynamic execution of PyTorch operations allows enough flexibility to change
computational graphs on the fly. This provides an avenue for Hy, a lisp-binding
library for Python, to be used in establishing meta-programming practices in the
field of deep learning.

While the final goal of this project is to build a framework for DL systems to have
access to their own coding, this coding paradigm also shows promise at accelerating
the development of new deep learning models while allowing significant manipulation
of low-torch tensor operations at runtime. A common trend in current DL packages is
an abundance of object-oriented abstraction with packages such as Keras. This only reduces
transparity to the already black-box nature of NN systems, and makes interpretability
 and reproducibility of models even more difficult.

In order to better understand NN models and allow for quick iterative design
over novel or esoteric architectures, a deep learning programmer requires access to an
environment that allows low-level definition of tensor graphs and provides methods
to quickly access network components for analysis, while still providing a framework
to manage large architectures. I believe that the added expressability of Lisp
in combination with PyTorch's functional API allows for this type of programming
paradigm, and provides DL researchers an extendable framework which cannot be
matched by any other abstracted NN packages.

## Features

### Pytorch Models as Hy-Expressions
Defining network components using Hy-expressions allows for modular design, quick iterative
refactoring, and manipulation of network code using macros. Here is a short example
of defining a computational graph using HyTorch tools and running forward propagation with
randomly initialized weight tensors.

```hy
; Importing Hytorch Tools and PyTorch
(import [hytorch.core [|gensym]])
(require [hytorch.core [|setv]])
(require [hytorch.thread [*]])
(import [hytorch.lisp [printlisp]])
(import torch)
(import [torch.nn.functional :as tfun])

; Checking for available cuda device
(setv device (torch.device (if (.is_available torch.cuda) "cuda:0" "cpu")))

; Defining leaf tensors
(setv leaf-tensor-defs '[(torch.empty [10] :dtype torch.float32 :requires-grad True)
                         (torch.empty [10 10] :dtype torch.float32 :requires-grad True)
                         (torch.empty [10] :dtype torch.float32 :requires-grad True)
                         (torch.empty [1 10] :dtype torch.float32 :requires-grad True)
                         (torch.empty [1] :dtype torch.float32 :requires-grad True)])

; Generate symbols for tensor-defs
(setv leaf-tensors (|gensym leaf-tensor-defs "L_"))

; Define assign expressions for leafs
(setv create-leafs `(|setv ~leaf-tensors ~leaf-tensor-defs))

; Define intialization procedures
(setv tensor-inits '[(-> torch.nn.init.normal (.to device))
                     (-> torch.nn.init.normal (.to device))
                     (-> torch.nn.init.normal (.to device))
                     (-> torch.nn.init.normal (.to device))
                     (-> torch.nn.init.normal (.to device))])

; Define init procedure application to leafs
(setv init-leafs (macroexpand `(|-> ~leaf-tensors ~tensor-inits)))

; Generate symbols for initialized weights
(setv w-tensors (|gensym leaf-tensor-defs "W_"))

; Define assign expressions for weights
(setv init-weights `(|setv ~w-tensors ~init-leafs))

; Defining a simple feed-forward NN as an S-Expression
(setv nn-def '(-> W_0
                  (tfun.linear W_1 W_2)
                  tfun.sigmoid
                  (tfun.linear W_3 W_4)
                  tfun.sigmoid))

; Define network parameter init procedure
(defmacro init-params []
  '(do  (eval create-leafs)
        (eval init-weights)))

; Initiate Parameters
(init-params)

; Running Forward Prop
(setv out (eval nn-def))

```
### Hy-Expression Threading
HyTorch contains custom threading macros to help define more complex network
architectures.

Broadcast Threading:
```hy
; Head Broadcast Threading
(print-lisp (macroexpand '(*-> [input1 input2] tfun.matmul (tfun.add bias) [tfun.sigmoid tfun.relu])))

; Tail Broadcast Threading
(print-lisp (macroexpand '(*->> [input1 input2] tfun.matmul (tf.add bias) [tfun.sigmoid tfun.relu])))
```
Output:
```
[(tfun.sigmoid (tfun.add (tfun.matmul input1 input2) bias)) (tfun.relu (tfun.add (tfun.matmul input1 input2) bias))]
[(tfun.sigmoid (tf.add bias (tfun.matmul input1 input2))) (tfun.relu (tf.add bias (tfun.matmul input1 input2)))]
```
List Inline Threading:
```hy
; Head List Inline Threading
(print-lisp (macroexpand '(|-> [input1 input2] [(tfun.linear w1 b1) (tf.linear w2 b2)] tfun.sigmoid)))

; Tail List Inline Threading
(print-lisp (macroexpand '(|->> [input1 input2] [(tfun.linear w1 b1) (tf.linear w2 b2)] tfun.sigmoid)))
```
Output:
```
[(tfun.sigmoid (tfun.linear input1 w1 b1)) (tfun.sigmoid (tf.linear input2 w2 b2))]
[(tfun.sigmoid (tfun.linear w1 b1 input1)) (tfun.sigmoid (tf.linear w2 b2 input2))]
```

### Pattern Matching
Hy-expression notation allows for pattern-matching over network definitions.

```hy
; Import Pattern Matching Functions and Macros
(import [hytorch.match [pat-match? pat-find]])
(require [hytorch.match [pat-refract]])

; pat-match? expr pattern
(pat-match? '(print (+ (+ 1 2) 3)) '(print (+ :HyExpression 3)))

; Match by parent-class association (ex. hy.model.HyExpression child of hy)
(pat-match? '(print (+ (+ 1 2) 3)) '(print (+ :hy 3)))

; Match by sub classes (ex. Hy.models)
(pat-match? '(print (+ (+ 1 2) 3)) '(print (+ :hy:models 3)))

; Network defintion
(setv nn-def '(-> W_0
                  (tfun.linear W_1 W_2)
                  tfun.sigmoid
                  (tfun.linear W_3 W_4)
                  tfun.sigmoid))

; Pattern match search over expanded defintion and return first 2
(for [match (pat-find (macroexpand nn-def) '(tfun.sigmoid :HyExpression) :n 2)]
     (print "Match:")
     (print-lisp match))

```
Output:
```
Match:
(tfun.sigmoid (tfun.linear (tfun.sigmoid (tfun.linear W_0 W_1 W_2)) W_3 W_4))
Match:
(tfun.sigmoid (tfun.linear W_0 W_1 W_2))
```

### Hy-Expression Refactoring
Quick and simple architectures refactoring without rewriting network code.

```hy
; Network defintion
(setv nn-def '(-> W_0
                  (tfun.linear W_1 W_2)
                  tfun.sigmoid
                  (tfun.linear W_3 W_4)
                  tfun.sigmoid))

; Refactor model definition
(print-lisp (macroexpand `(pat-refract ~(macroexpand nn-def) (tfun.sigmoid tfun.relu)
                                                             (W_2 B_2)
                                                             (W_4 B_4))))
```
Output:
```
(tfun.relu (tfun.linear (tfun.relu (tfun.linear W_0 W_1 B_2)) W_3 B_4))
```

## Installation:

#### Dependencies:

The current project has been tested using Hy 0.16.0, PyTorch 1.0.1.post2 and
Python 3.7.

The following ***Pip*** command can be used to install **HyTorch**:

```
$ pip3 install git+https://github.com/rz4/HyTorch
```

## Jupyter Notebook Setup

Ok, so this is already awesome. Let's make it even better by running HyTorch in
a Jupyter notebook!

### Calysto - Hy IPython Kernel

I've taken the current version of [calysto_hy](https://github.com/Calysto/calysto_hy)
and made some changes to be able to run it with the latest version of Hy (0.16.0). Even though
this version runs the kernel without major errors, the autocomplete feature is still not working
and I will be looking into fixing this in the future.

To make this kernel available to Jupyter run the following command:

```
$ cd jupyter
/jupyter/$ python3 setup.py install
/jupyter/$ python3 -m calysto_hy install --sys-prefix

```

### ParInfer Notebook Extension

Since this project is intended to showcase the power of Lisp, I found an notebook extension
for [ParInfer](https://github.com/shaunlebron/parinfer) in this great Clojure Jupyter kernel
[repository](https://github.com/clojupyter/lein-jupyter). Many people stay away
from Lisps due to the abundance of parenthesis, but ParaInfer takes away the need to
manage all those annoying curves. I would recommend reading the documentation of
ParInfer to get a good feel on how it works. The extension can be easily turned
on and off in the notebook, although it is **not currently available when running
notebooks using JupyterLab**.

The extension has been refactored to work with the Calysto kernel and can be
installed and enabled using the following commands:

```
$ cd jupyter
/jupyter/$ jupyter nbextension install calysto-parinfer --user
/jupyter/$ jupyter nbextension enable calysto-parinfer/index.js --user

```

## Documentation and Tutorials

An introduction to Hytorch and some use cases can be found in the following
Jupyter notebooks:

- [HyTorch Tutorial](notebooks/HyTorch_Tutorial.ipynb)
