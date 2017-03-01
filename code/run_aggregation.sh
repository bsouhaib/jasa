#!/bin/bash

rscript="aggregation.R"

  #njobs=64
  #nperjobs=4416/$njobs
njobs=36
nperjobs=123
  #nperjobs=4416/$njobs

#allijobs=$(seq 1 $njobs )

allijobs=( 1 2 3 4 24 )

for ijob in ${allijobs[@]}
do
  start=$(( 0 + ($ijob - 1) * $nperjobs + 1 ))
  end=$(( 0 + ($ijob - 1) * $nperjobs + $nperjobs ))
  allidtest=( $(seq $start $end ) )
  
  echo "${allidtest[@]}"
  Rscript --vanilla $rscript $ijob ${allidtest[@]} > "/home/rstudio/PROJ/rout/aggregation-$ijob.Rout" 2> "/home/rstudio/PROJ/rout/aggregation-$ijob.err" &
done


