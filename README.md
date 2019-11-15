![LOGO](images/name.png)
# (setv MinoTauro (+ Hy PyTorch))
PyTorch Meta-Programming Using the Lisp Dialect Hy

![Current Version](https://img.shields.io/badge/version-0.0.3-red.svg)

Lead Maintainer: [Rafael Zamora-Resendiz](https://github.com/rz4)

**MinoTauro** is a Hy (0.17.0) library running Python (3.7) and PyTorch (1.0.1)
for use in rapid low-level development of differential programs as well as
for experiments in deep learning meta-programming.

## Motivation
The dynamic execution of PyTorch operations allows enough flexibility to change
computational graphs on the fly. This provides an avenue for Hy, a lisp-binding
library for Python, to be used in establishing meta-programming practices in the
field of deep learning (DL).

While the final goal of this project is to build a framework for DL systems to have
access to their code, this coding paradigm also shows promise at accelerating
the development of new differential models while promoting formalized abstraction
with type checking. A common trend in current DL packages is
an abundance of object-oriented abstraction with packages such as Keras. This only reduces
transparency to the already black-box nature of NN systems, and makes interpretability
and reproducibility of models more difficult.

In order to better understand DL models and allow for quick iterative design
over novel or esoteric architectures, programmers require access to an
environment which allows low-level definition of tensor graphs and provides methods
to quickly access network components for debugging and analysis, while using Pytorch's
backend to run on high performance machines. I believe that the added expressibility of Lisp
in combination with PyTorch's functional API allows for this type of programming
paradigm, and provides DL researchers an extendable framework which cannot be
matched by any other abstracted NN packages.

## Features

### Pytorch Models as Hy-Expressions
Defining models using Hy-Expressions allows for functional design, quick iterative
refactoring, and manipulation of model code using macros. Here is a short example
of defining a single layer feed forward neural network using MinoTauro's tools and training
the model on dummy data.

```hy
; Imports
(import torch
        [torch.nn.functional :as F]
        [torch.nn [Parameter]]
        [torch.optim [Adam]])

; Macros
(require [minotauro [*]] ; defsigmod
         [minotauro.thread [*]] ; Threading macros :{->,->>,*->,...}
         [hy.contrib.walk [let]])

;; Linear Function Module
(defsigmod Linear [x w b]
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
(defsigmod FeedForwardNN [x fc-in act fc-out]
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

MinoTauro works alongside PyTorch's abstractions providing a more functional style for
defining computational graphs. MinoTauro also includes a library of specialized threading
macros to define more complex models.

In the above example, we show the use of the macro `defsigmod` which takes its arguments and
defines a PyTorch `Module` class. The `components` used by the module during forward
propagation are defined in the argument list. The first expression following the argument list
defines the `forward-procedure`. Thus, defining a PyTorch module takes the following form:

```hy
(defsigmod sigmod-name [component-0 ...] forward-procedure)
```

While PyTorch's module system uses a object oriented approach, MinoTauro's abstractions allows
for functional manipulation. MinoTauro abstracts the PyTorch `Module` into the form `sigmod`.
A `sigmod` can be thought of as a lambda expression with all the added benefits of PyTorch's
`Module` system. Default `components` (or sub-modules in traditional PyTorch) can be
binded to `sigmod`s when creating a new object. If bound during initialization, the
default `components` will be used during the forward pass.

For example, the `n/Linear` function in the code example creates a new `Linear` module with custom
default-and-persistent tensors, weight `w` and bias `b`. If arguments `w` or `b` are not provided
during the forward pass of `Linear`, then these default values are used.

### Anonymous Sigmods (i.e. Anonymous PyTorch Modules)
Side effects make systems harder to debug and understand. The `sigmod` was designed to
limit the `Module` to a formalized abstraction similar to the lambda. MinoTauro allows
for anonymous PyTorch `Modules` through `sigmod`. For example, an anonymous Linear function
can be defined as follows:

```hy
; Anonymous Linear
(sigmod [x w b] (-> x (* w) (+ b)))

; Forward Propagate
((sigmod [x w b] (-> x (* w) (+ b))) my-x my-w my-b)

```

MinoTauro's macro `bind` can be used to assign default values to components same as when creating
a new object of a namespaced `sigmod` with `defsigmod`. Using the Linear function as an example again:

```hy
; Anonymous Linear with default w and b
(bind (sigmod [x w b] (-> x (* w) (+ b)))
  :w (-> (torch.empty (, f-in f-out))
         (.normal_ :mean 0 :std 1.0)
         (Parameter :requires_grad True))
  :b (-> (torch.empty (, f-out))
         (.normal_ :mean 0 :std 1.0)
         (Parameter :requires_grad True))))

; Namespaced Linear with default w and b
(Linear
  :w (-> (torch.empty (, f-in f-out))
         (.normal_ :mean 0 :std 1.0)
         (Parameter :requires_grad True))
  :b (-> (torch.empty (, f-out))
         (.normal_ :mean 0 :std 1.0)
         (Parameter :requires_grad True))))
```


### Hy-Expression Threading
MinoTauro contains custom threading macros to help define more complex network
architectures. Threading macros are common to other function-heavy lisp languages such as Clojure.
The simplest comes in the form of the thread first macro `->`, which inserts each expression into the
next expression’s first argument place. The compliment of this macro which inserts the expressions
into the last argument place is `->>`. The following are various threading macros available in
MinoTauro:

#### Broadcast-thread first macro:
Variation of the thread first macro which threads listed forms as
the first n arguments in the next form.
Example:
``` hy
(*-> [x y] +)

; Returns
(+ x y)
```
When a form precedes a list of forms broadcast the preceding form to each form in the list.
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
If a list of forms precedes a single form, thread form along each branch.
Example:
``` hy
(|-> [x y] (+ 1))

; Returns
[(+ x 1) (+ y 1)]
```

#### Set-thread first macro:
Variation of the thread first macro which stores the result at each form thread.
Used when its more efficient to pass pointer of the evaluated form to next form operation instead
of an unevaluated form. Gensym is used for variable names to prevent namespace collisions.
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

It's important to note that all threading macros provided in MinoTauro macroexpand each form
prior to threading it into the next expression. As standard, the "last" version of each macro is the
symbol plus an added `>` (i.e `*->>`, `|->>`, etc.)

## Spec (Clojure-like Specifications For PyTorch)
Inspired from Clojure's `spec`, MinoTauro includes a similar system for predicate type checking.
This package in conjunction with the formalized `sigmod` allows for runtime predicate checking of components,
and other features common in Clojure's `spec` such as `conform`, `explain`, and `gen` (COMING SOON).
These tools were added to MintoTauro to help debugging computational graphs, constrain model architecture
to facilitate design collaboration, and to generate valid data/models. Here is a small peak into these features:

```hy
; Macros
(require (minotauro.spec [*]))

; Define Specification
(spec/def :nb-even even?)

; More Complex Specifications
(spec/def :int-even (spec/and (fn [x] (instance? int x)) :nb-even))

; Runtime Specification Check
(spec/valid? :int-even 2)

;; Returns -> True
```

## Installation:

#### Dependencies:

The current project has been tested using Hy 0.16.0, PyTorch 1.0.1.post2 and
Python 3.7.

The following ***Pip*** command can be used to install **MinoTauro**:

```
$ pip3 install git+https://github.com/rz4/MinoTauro
```

## Jupyter Notebook Setup

Ok, so this is already awesome. Let's make it even better by running MinoTauro in
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
