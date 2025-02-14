#!/bin/bash

source SETUP_CHICKEN_LOCAL_MODULES.sh

# terry provides for-helper with-symbols
csc -s -J terry.scm

# for-tidy uses for-helper with-symbols
#csc -s -J for-tidy.scm
csc -s -J for-pro.scm


# demo something that uses for
rm -f demo
csc -o demo demo.scm

