#!/bin/bash

for i in 2 4 8 16 32 64 128 256
do
    echo "Generating $i" 
    ./generate-kcfa-worst-case.scm $i > ../benchmarks/kcfa-worst-case-$i.scm 
done