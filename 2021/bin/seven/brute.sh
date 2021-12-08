#!/bin/sh
#@(#) solve problem seven by brute force
#@(#) basically, we iterate through the whole set of positions
#@(#) and return which one is the lowest cost
for i in `seq 0 999`
do
echo "starting $i" >> dump
dune exec bin/seven/seven.exe data/seven/test1 $i >> dump
echo "done with $i" >> dump
done
