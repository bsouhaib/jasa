#!/bin/bash

rscript="aggregation.R"

#njobs=100
#nperjobs=45

  #njobs=35
  #nperjobs=127
  #nperjobs=4416/$njobs
#njobs=34
#nperjobs=130

#njobs=32
#nperjobs=138


#njobs=12
#nperjobs=368

njobs=12
nperjobs=368

allijobs=$(seq 1 $njobs )
allijobs=(3 5)

for ijob in ${allijobs[@]}
do
  start=$(( 0 + ($ijob - 1) * $nperjobs + 1 ))
  end=$(( 0 + ($ijob - 1) * $nperjobs + $nperjobs ))
  allidtest=( $(seq $start $end ) )
  
  echo "${allidtest[@]}"
  Rscript --vanilla $rscript $ijob ${allidtest[@]} > "/home/rstudio/PROJ/rout/aggregation-$ijob.Rout" 2> "/home/rstudio/PROJ/rout/aggregation-$ijob.err" &
done


