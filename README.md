![LOGO](images/logo.png)
# (setv HyTorch (+ Hy PyTorch))
PyTorch Meta-Programming Using the Lisp Dialect Hy

![Current Version](https://img.shields.io/badge/version-0.0.0-red.svg)

Lead Maintainer: [Rafael Zamora-Resendiz](https://github.com/rz4)

**HyTorch** is a Hy (0.16.0) library running Python (3.7) and Pytorch (1.0.1)
for use in rapid low-level development of deep learning systems as well as
for experiments in DL meta-programming.

## Motivation
The dynamic execution of Pytorch operations allows enough flexibity to change
computational graphs on the fly. This provides an avenue for Hy, a lisp-binding
library for Python, to be used in establishing meta-programming practices in the
field of deep learning.

While the final goal of this project is to build a framework for DL systems to have
access to their own coding, this coding paradigm
also shows promise at accelerating the development of new deep learning models
while providing significant access to low-torch tensor operations at runtime.
A common trend in current DL packages is an abundance of object-oriented abstraction with
packages such as Keras. This only reduces transparity to the already black-box nature of NN
systems, and makes reproducibilty of models even more difficult.

In order to better understand NN models and allow for quick iterative design
over novel or esoteric architectures, a deep learning programmer requires access to an
environment that allows low-level definition of tensor graphs and provides methods to quickly access network
components for analysis, while still providing a framework to manage large architectures. I
believe that the added expressability of Lisp in combination with PyTorch's functional API allows for this type of
programming paradigm, and provides DL researchers an extendable framework which cannot be matched by any other
abstracted NN packages.

## Features
TODO

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
for [ParInfer](https://github.com/shaunlebron/parinfer) in this great Clojure Jupyter
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
