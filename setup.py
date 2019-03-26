#!/usr/bin/env python3

__author__ = 'Rafael Zamora-Resendiz, rz4@hood.edu'

from setuptools import setup, find_packages

setup(
    name="hytorch",
    version="0.0.0",
    description="PyTorch Manipulation Using the Lisp Dialect Hy",
    license="MIT",
    keywords="Hy Pytorch",
    packages=find_packages(exclude=["images", "jupytor", "notebooks"]),
    install_requires = ["torch==1.0.1.post2", "hy==0.16.0"],
)
