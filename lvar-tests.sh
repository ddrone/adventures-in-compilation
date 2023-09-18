#!/usr/bin/env bash

for f in tests/*.lvar
do
  ./run-lvar.sh $f
done