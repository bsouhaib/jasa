#!/bin/bash

rscript="aggregation.R"

if [[ "$1" -eq 1 ]]; then
allidtest=$(seq 1 1104 )
elif [[ "$1" -eq 2 ]]; then
allidtest=$(seq 1105 2208 )
elif [[ "$1" -eq 3 ]]; then
allidtest=$(seq 2209 3312 )
elif [[ "$1" -eq 4 ]]; then
allidtest=$(seq 3313 4416 )
fi


#echo "${allidtest[@]}"

Rscript --vanilla $rscript $1 ${allidtest[@]} > "/home/rstudio/PROJ/rout/aggregation-$1.Rout" 2> "/home/rstudio/PROJ/rout/aggregation-$1.err"

