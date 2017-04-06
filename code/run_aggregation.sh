#!/bin/bash

rscript="aggregation.R"

#njobs=100
#nperjobs=45

  #njobs=35
  #nperjobs=127
  #nperjobs=4416/$njobs
#njobs=34
#nperjobs=130
njobs=32
nperjobs=138

#njobs=4
#nperjobs=1104

#njobs=62
#nperjobs=72

allijobs=$(seq 1 $njobs )
allijobs=(2 4 7 10 20 27 32 33 34)

for ijob in ${allijobs[@]}
do
  start=$(( 0 + ($ijob - 1) * $nperjobs + 1 ))
  end=$(( 0 + ($ijob - 1) * $nperjobs + $nperjobs ))
  allidtest=( $(seq $start $end ) )
  
  echo "${allidtest[@]}"
  Rscript --vanilla $rscript $ijob ${allidtest[@]} > "/home/rstudio/PROJ/rout/aggregation-$ijob.Rout" 2> "/home/rstudio/PROJ/rout/aggregation-$ijob.err" &
done


