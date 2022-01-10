#!/bin/bash

cd fut
futhark $1 --library algebra.fut
cd ..
futhask $1 fut/algebra.h src Algebra
stack build

