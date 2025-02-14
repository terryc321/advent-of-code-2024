#!/bin/bash

# source this file in bash
# source SETUP_CHICKEN_LOCAL_MODULES.sh


export CHICKEN_REPOSITORY_PATH=/usr/local/lib/chicken/11:`pwd`/../../macros

# csc -s -J macros.scm
# produces shared library and an import file 



# csc -s macros.scm

# csc -s macros.scm
# generates a shared library
#
# complains about define-macro (for e . body)
# perhaps a compound expression that needs disassembly
#
# which can be loaded
# > ,l macros.so
#
# > (import macros)
# ;; loading ...
#
# (let ((a 1)(b 2)) (swap! a b) (list a b))
# macrros do not work either
#
# (define-macro (incf! x)
#  `(set! ,x (+ ,x 1)))
#
# complains about unbound variable x
#
#
# can we do explicit renaming ?
#


