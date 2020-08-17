![LOGO](docs/_static/imgs/logo.png)
# (spec/def :MinoTauro (spec/and Hy? PyTorch?))

*If all the ways I have been along were marked on a map and joined up with a line, it might represent a minotaur.*

*- Pablo Picasso*

PyTorch Meta-Programming Using the Lisp Dialect Hy

![Current Version](https://img.shields.io/badge/version-0.1.0-green.svg)

Lead Maintainer: [Rafael Zamora-Resendiz](https://github.com/rz4)

**MinoTauro** is a Hy (0.19.0) library running Python (3.8) and PyTorch (1.3)
for use in rapid bottom-up development of differential programs as well as
for experiments in deep learning meta-programming.

- MinoTauro lets you implement lambda-esk 'anonymous PyTorch Modules' referred to as 'mu expressions'.
- MinoTauro lets you bind parameters and modules to a mu expression.
- MinoTauro promotes Pythonic data accessing calls.
- MinoTauro allows you to revert a models back to an S-Expressions.
- And it all remains compatible with native PyTorch!

## Documentation

We are currently working on migrating all MinoTauro's documentation to
a new GitHub page. It is under active development, so keep coming back every now and then
to see updates.

You can view the API here:

[https://rz4.github.io/MinoTauro](https://rz4.github.io/MinoTauro)


## Installation:

#### Dependencies:

The current project has been tested using Hy 0.19.0, PyTorch 1.3.1 and
Python 3.8.

The following ***Pip*** command can be used to install **MinoTauro**:

```
$ pip3 install git+https://github.com/rz4/MinoTauro
```

## Jupyter Notebook Setup

Let's make it even better by running MinoTauro in a Jupyter notebook.

### Calysto - Hy IPython Kernel

We've taken the current version of [calysto_hy](https://github.com/Calysto/calysto_hy)
and made some changes to be able to run it with the latest version of Hy (0.19.0). Even though
this version runs the kernel without major errors, the autocomplete feature is still not working
and I will be looking into fixing this in the future.

To make this kernel available to Jupyter run the following command:

```
$ cd jupyter
/jupyter/$ python3 setup.py install
/jupyter/$ python3 -m calysto_hy install --sys-prefix

```

### ParInfer Notebook Extension

Since this project is intended to showcase the power of Lisp, we found an notebook extension
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
