#!/usr/bin/env bash

for f in tests/*.py
do
  ./run-lvar.sh $f
done