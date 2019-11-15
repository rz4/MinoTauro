#!/usr/bin/env python3

__author__ = 'Rafael Zamora-Resendiz, rzamoraresendiz@protonmail.com'

from setuptools import setup, find_packages

setup(
    name="minotauro",
    version="0.0.0",
    description="PyTorch Meta-Programming Using the Lisp Dialect Hy",
    license="MIT",
    keywords="Hy Pytorch",
    packages=find_packages(exclude=["images", "jupytor", "notebooks"]),
    package_data={
        'minotauro': ['*.hy',],
    },
    install_requires = ["hy==0.17.0" "torch"],
)
