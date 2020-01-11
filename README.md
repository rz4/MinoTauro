![LOGO](images/name.png)
# (spec/def :MinoTauro (spec/and Hy? PyTorch?))

*If all the ways I have been along were marked on a map and joined up with a line, it might represent a minotaur.*

*- Pablo Picasso*

PyTorch Meta-Programming Using the Lisp Dialect Hy

![Current Version](https://img.shields.io/badge/version-0.0.5-red.svg)

Lead Maintainer: [Rafael Zamora-Resendiz](https://github.com/rz4)

**MinoTauro** is a Hy (0.17.0) library running Python (3.7) and PyTorch (1.3)
for use in rapid low-level development of differential programs as well as
for experiments in deep learning meta-programming.

## Motivation
The dynamic execution of PyTorch operations allows enough flexibility to change
computational graphs on the fly. This provides an avenue for Hy, a lisp-binding
library for Python, to be used in establishing meta-programming practices in the
field of differential learning (DL).

While the final goal of this project is to build a framework which will allow
DL systems to have access to their code during runtime, this coding paradigm
also shows promise at accelerating the development of new differential models
while promoting formalized abstraction with predicate type checking. A common
trend in current DL packages is an abundance of opaque object-oriented abstraction
with packages such as Keras. This only reduces transparency to the already
black-box nature of neural network (NN) systems, and makes interpretability and
reproducibility of models more difficult.

In order to better understand DL models and allow for quick iterative design
over novel or esoteric architectures, programmers require access to an
environment which allows low-level definition of computational graphs and
provides methods to quickly access network components for debugging and analysis,
while still providing gpu-acceleration. The added expressibility of Lisp in
combination with PyTorch's functional API allows for this type of programming
paradigm, and provides DL researchers an extendable framework which cannot be
matched by the restrictive set of abstractions allowed in contemporary NN packages.

## Features

### Computational Graphs as S-Expressions
Defining models using S-Expressions allows for functional design, quick iterative
refactoring, and manipulation of model code using macros. Here is a short example
of defining a single layer feed forward neural network using MinoTauro and
then training a generated model on dummy data.

```hy
; Imports
(import torch
        [torch.nn.functional :as F]
        [torch.nn :as nn]
        [torch.optim [Adam]])

; Requires
(require [minotauro.mu [*]]
         [minotauro.thread [*]]
         [hy.contrib.walk [let]])

;; Define a Linear Transformation operation:
(defmu LinearTransformation [x weights bias]
  (-> x (@ weights) (+ bias)))

;; Define a constructor for a Linear Transformation with Learnable Parameters
(defn LearnableLinear [f-in f-out]
  (LinearTransformation
    :weights (-> (torch.empty (, f-in f-out))
                 (.normal_ :mean 0 :std 1.0)
                 (nn.Parameter :requires_grad True))
    :bias    (-> (torch.empty (, f-out))
                 (.normal_ :mean 0 :std 1.0)
                 (nn.Parameter :requires_grad True))))

;; Define a Single-layer Feed Forward Neural Network
(defmu FeedForwardNeuralNetwork [x linear-to-hidden linear-to-output
  (-> x
      linear-to-hidden
      torch.sigmoid
      linear-to-output))

;; Define FeedForwardNeuralNetwork Spec Generator
(defn FFNN [nb-inputs nb-hidden nb-outputs]
  (FeedForwardNeuralNetwork
    :linear-to-hidden (LearnableLinear nb-inputs nb-hidden)
    :linear-to-output (LearnableLinear nb-hidden nb-outputs)

;; main -
(defmain [&rest _]

  (print "Loading Model + Data...")
  (let [nb-inputs 10 nb-hidden 32 nb-outputs 1]

    ; Define Model + Optimizer
    (setv model (FFNN nb-inputs nb-hidden nb-outputs)
          optimizer (Adam (.parameters model) :lr 0.001 :weight_decay 1e-5))

    ; Generate Dummy Data
    (let [batch-size 100]
      (setv x (-> (torch.empty (, batch-size nb-inputs))
                  (.normal_ :mean 0 :std 1.0))
            y (torch.ones (, batch-size nb-outputs))))

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
PyTorch auto-differential system
works through definitions of models as `Modules` which are used to organize
operations and dependent learnable parameters.
MinoTauro extends PyTorch's abstractions by letting you define computational graphs
in functional-syntax through Minotauro's `mu` expressions.  
In short, Minotauro makes writing new modules as simple as writing a new lambda expression.

In the above example, we show the use of the macro `defmu` which takes its arguments and
defines a PyTorch `Module` class. The `components` used by the module during forward
propagation are defined in the argument list. The expressions following the argument list
defines the `forward-procedure`. `components` are typically `torch.nn.Parameter`s or
`torch.nn.Module`s.

Thus, defining a PyTorch module takes the following form:

```hy
(defmu module-name [component-0 ... component-N] forward-procedure)
```

While PyTorch's module system uses an object oriented approach, MinoTauro's abstractions allows
for functional manipulation of tensor objects. MinoTauro abstracts the PyTorch `Module` into
the form `mu`. A `mu` can be thought of as a lambda expression with all the added
benefits of PyTorch's `Module` system. This means all native PyTorch operations
still work including moving PyTorch objects to and from devices and accessing sub-modules
and parameters.

Default `components` (or sub-modules in traditional PyTorch) can be binded to `mu`s when
creating a new object. If bound during initialization, the default `components`
will be used during the forward pass if not provided in the function call.

As an example, the `LearnableLinear` function in the code example generates a new
`LinearTransformation` module with custom default-and-persistent tensors, `weights`
and `bias`. If arguments `weights` or `bias` are not provided
during the forward pass of `LinearTransformation`, then these default values are used instead.

We can view a representation of the computational graph by printing the model.
```
(print model)

; Returns

FeedForwardNeuralNetwork(
  ID: -G\uffff7
  C: [x linear-to-hidden linear-to-output]
  λ: (linear-to-output (torch.sigmoid (linear-to-hidden x)))

  (linear_to_hidden): LinearTransformation(
    ID: -G\uffff5
    C: [x weights bias]
    λ: (+ (@ x weights) bias)

    weights: Parameter(size: [10, 32] dtype: torch.float32)
    bias: Parameter(size: [32] dtype: torch.float32)

  )
  (linear_to_output): LinearTransformation(
    ID: -G\uffff6
    C: [x weights bias]
    λ: (+ (@ x weights) bias)

    weights: Parameter(size: [32, 1] dtype: torch.float32)
    bias: Parameter(size: [1] dtype: torch.float32)

  )
)
```

Accessing and viewing any component of the model is simple and done through python's dot notation.
This make exploring a model easy and intuitive.
```
(print model.linear-to-hidden)

; Returns

LinearTransformation(
  ID: -G\uffff1
  C: [x weights bias]
  λ: (+ (@ x weights) bias)

  weights: Parameter(size: [10, 32] dtype: torch.float32)
  bias: Parameter(size: [32] dtype: torch.float32)

)
```

### Anonymous Sigmods (i.e. Anonymous PyTorch Modules)
Side effects make systems harder to debug and understand. The `mu` was designed to
limit the `Module` to a formalized abstraction similar to lambda expressions. MinoTauro allows
for anonymous PyTorch `Modules` through `mu`. For example, an anonymous Linear function
can be defined as follows:

```hy
; Anonymous Linear
(mu [x w b] (-> x (@ w) (+ b)))

; Forward Propagate
((mu [x w b] (-> x (@ w) (+ b))) my-x my-w my-b)

```

MinoTauro's macro `bind` can be used to assign default values to components same as when creating
a new object of a namespaced `mu` with `defmu`. Using the Linear function as an example again:

```hy
; Anonymous Linear with default w and b
(bind (mu [x w b] (-> x (@ w) (+ b)))
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
Threading here refers to the practice of passing arguments through expression not concurrency.
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
This package in conjunction with the formalized `mu` allows for runtime predicate checking of components,
and other features common in Clojure's `spec` such as `conform`, `explain`, and `gen`.
These tools were added to MintoTauro to help debug computational graphs, constrain model architecture
to facilitate design collaboration, and to easily generate valid data/models.
Here is an example of defining a data specification for the previous neural network example:

```hy
; Imports
(import torch
        [torch.nn.functional :as F]
        [torch.nn :as nn]
        [torch.optim [Adam]])

; Requires
(require [minotauro.mu [*]]
         [minotauro.thread [*]]
         [minotauro.spec [*]]
         [hy.contrib.walk [let]])

;; Define PyTorch Object Specifications
(spec/def :tensor (fn [x] (instance? torch.Tensor x))
          :learnable (fn [x] x.requires_grad)
          :rank1 (fn [x] (-> x .size len (= 1)))
          :rank2 (fn [x] (-> x .size len (= 2))))

;; Linear Operation
(defmu LinearTransformation [x weights bias]
  (-> x (@ weights) (+ bias)))

;; Define a Learnable LinearTransformation data specification:
;; Weights and bias are contrained to ensure model is correctly configured.
;; In this instance, weights must conform to Tensors with learnable gradients of ranks 2 and 1 respectively.
(spec/def :LearnableLinear (spec/parameters weights (spec/and :tensor :learnable :rank2)
                                            bias    (spec/and :tensor :learnable :rank1)))

;; Define a generator for the LearnableLinear Specification:
;; Generators are lambda expressions which will return data that conforms to the specification.
(spec/defgen :LearnableLinear [f-in f-out]
  (LinearTransformation :weights (-> (torch.empty (, f-in f-out))
                                     (.normal_ :mean 0 :std 1.0)
                                     (nn.Parameter :requires_grad True))
                        :bias    (-> (torch.empty (, f-out))
                                     (.normal_ :mean 0 :std 1.0)
                                     (nn.Parameter :requires_grad True))))

;; Single-layer Feed Forward Neural Network
(defmu FeedForwardNeuralNetwork [x linear-to-hidden linear-to-output]
  (-> x
      linear-to-hidden
      torch.sigmoid
      linear-to-output))

;; Define FeedForwardNeuralNetwork Specification
(spec/def :FeedForwardNeuralNetwork (spec/modules linear-to-hidden :LearnableLinear
                                                  linear-to-output :LearnableLinear))

;; Define FeedForwardNeuralNetwork Spec Generator
(spec/defgen :FeedForwardNeuralNetwork [nb-inputs nb-hidden nb-outputs]
  (FeedForwardNeuralNetwork :linear-to-hidden (spec/gen :LearnableLinear nb-inputs nb-hidden)
                            :linear-to-output (spec/gen :LearnableLinear nb-hidden nb-outputs)))

;; main -
(defmain [&rest _]

  (print "Loading Model + Data...")
  (let [nb-inputs 10 nb-hidden 32 nb-outputs 1]

    ; Define Model + Optimizer
    (setv model (spec/gen :FeedForwardNeuralNetwork nb-inputs nb-hidden nb-outputs)
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

The neural network example uses `spec` to
define valid configurations of the `LearnableLinear` and `FeedForwardNeuralNetwork` modules.
These `spec` definitions makes it simple and concise to test that modules
have valid components. If a generator is defined for a `spec`, then
the generated data will be tested against its specification and fail when
the data does not conform.

## Installation:

#### Dependencies:

The current project has been tested using Hy 0.17.0, PyTorch 1.3.1 and
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
