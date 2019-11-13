![LOGO](images/logo.png)
# (setv HyTorch (+ Hy PyTorch))
PyTorch Meta-Programming Using the Lisp Dialect Hy

![Current Version](https://img.shields.io/badge/version-0.0.1-red.svg)

Lead Maintainer: [Rafael Zamora-Resendiz](https://github.com/rz4)

**HyTorch** is a Hy (0.17.0) library running Python (3.7) and PyTorch (1.0.1)
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
Defining models using Hy-Expressions allows for modular design, quick iterative
refactoring, and manipulation of model code using macros. Here is a short example
of defining a single layer feed forward neural network using HyTorch tools and training
on the model on dummy data.

```hy
; Imports
(import torch
        [torch.nn.functional :as F]
        [torch.nn [Parameter]]
        [torch.optim [Adam]])

; Macros
(require [hytorch [*]] ; defmodule
         [hytorch.thread [*]] ; Threading macros :{->,->>,*->,...}
         [hy.contrib.walk [let]])

;; Linear Function Module
(defmodule Linear [x w b]
  (-> x (@ w) (+ b)))

;; Linear Module Initializer
(defn n/Linear [f-in f-out]
  (Linear :w (-> (torch.empty (, f-in f-out))
                 (.normal_ :mean 0 :std 1.0)
                 (Parameter :requires_grad True))
          :b (-> (torch.empty (, f-out))
                 (.normal_ :mean 0 :std 1.0)
                 (Parameter :requires_grad True))))

;; Single-layer Feed Forward Neural Network
(defmodule FeedForwardNN [x fc-in act fc-out]
  (-> x fc-in act fc-out))

;; FFNN Module Initializer
(defn n/FeedForwardNN [nb-inputs nb-hidden nb-outputs]
  (FeedForwardNN :fc-in (n/Linear nb-inputs nb-hidden)
                 :fc-out (n/Linear nb-hidden nb-outputs)
                 :act torch.sigmoid))

;; main -
(defmain [&rest _]

  (print "Loading Model + Data...")
  (let [nb-inputs 10 nb-hidden 32 nb-outputs 1]
    ; Define Model + Optimizer
    (setv model (n/FeedForwardNN nb-inputs nb-hidden nb-outputs)
          optimizer (Adam (.parameters model) :lr 0.001 :weight_decay 1e-5))

    ; Generate Dummy Data
    (let [batch-size 100]
      (setv x (-> (torch.empty (, batch-size nb-inputs))
                  (.normal_ :mean 0 :std 1.0))

            y (torch.ones (, batch-size nb-outputs)))))

  ; Train
  (let [epochs 100]
    (print "Training...")
    (for [epoch (range epochs)]

      ; Forward
      (setv y-pred (model x))
      (setv loss (F.binary_cross_entropy_with_logits y-pred y))
      (print (.format "Epoch: {epoch} Loss: {loss}" :epoch epoch :loss loss))

      ; Backward
      (.zero_grad optimizer)
      (.backward loss)
      (.step optimizer))))
```

HyTorch works alongside PyTorch abstractions and allows for a more functional style of
programming computational graphs, and adds macro programming through threading functions to define more
complex models. In the above example, we show the use of the macro `defmodule` which takes the
arguments and defines a PyTorch `Module` object. The `components` used by the module during forward
propagation are defined in the argument list. The first expression following the argument list
defines the `forward procedure`. Thus, defining a PyTorch module takes the following form:

```hy
(defmodule module-name [component-0 ...] forward-procedure)
```

While PyTorch's module system uses a object oriented approach, HyTorch's abstractions allows
for more functional manipulation by only using locally stored variables if defined when instantiating
a new module object. For example, the `n/Linear` function creates a new `Linear` module with custom
default and persistent tensors, weight `w` and bias `b`. If arguments `w` or `b` are not provided
during the forward pass of `Linear`, then the default values are used.

### Hy-Expression Threading
HyTorch contains custom threading macros to help define more complex network
architectures. Threading macros are common to other functional lisp langauges such as Clojure.
The simplest comes in the form of the thread first macro `->`, which inserts each expression into the
next expression’s first argument place. The compliment of this macro which inserts the expressions
into the last argument place is `->>`. The following are various threading macros available in
HyTorch:

#### Broadcast-thread first macro:
Variation of the thread first macro which threads listed forms as
the first n arguments in the next form.
Example:
``` hy
(*-> [x y] +)

; Returns
(+ x y)
```
When form precedes list of forms broadcast preceding forms to each form in list.
Example:
``` hy
(*-> x [inc dec])

; Returns
[(inc x) (dec x)]
```

#### Inline-thread first macro:
Variation of the thread first macro which threads listed forms in parallel.
Example:
``` hy
(|-> [x y] [inc dec])

; Returns
[(inc x) (dec y)]
```
If list of forms precedes a single form, thread form along each branch.
Example:
``` hy
(|-> [x y] (+ 1))

; Returns
[(+ x 1) (+ y 1)]
```

#### Set-thread first macro:
Variation of the thread first macro which stores the result at each form thread.
Used when its more efficient to pass pointer of the evaluated form to next form operation instead
of unevaluated form. Gensym used for variable name to prevent namespace collisions.
Example:
```hy
(=-> (+ x 1) incr)

; Returns
(do (setv g!123 (+ x 1)) (setv g!123 (incr g!123)) g!123)
```

#### Conditional-thread first macro:
Variation of the thread first macro which operates as cond-> in Clojure.
Example:
```hy
(cond-> x True incr (even? 2) incr)

; Returns
(if True
  (if (even? 2)
    (incr (incr x)))
    (incr x))
 x)
```

It's important to note that all threading macros provided in HyTorch macroexpand each form
prior to threading into next expression. As standard, the "last" version of each macro is the
symbol plus an added `>` (i.e `*->>`, `|->>`, etc.)

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

- [HyTorch Tutorial](notebooks/HyTorch_Tutorial-old.ipynb)
