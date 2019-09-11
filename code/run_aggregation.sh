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

#njobs=12
#nperjobs=368

#allijobs=$(seq 1 $njobs )
allijobs=(11)

for ijob in ${allijobs[@]}
do
  #start=$(( 0 + ($ijob - 1) * $nperjobs + 1 ))
  #end=$(( 0 + ($ijob - 1) * $nperjobs + $nperjobs ))
  #allidtest=( $(seq $start $end ) )
  allidtest=(3290 3291 3292 3293 3294 3295 3296 3297 3298 3299 3300 3301 3302 
  3303 3304 3305 3306 3307 3308 3309 3310 3311 3312)
  
  echo "${allidtest[@]}"
  Rscript --vanilla $rscript $ijob ${allidtest[@]} > "/home/rstudio/PROJ/rout/aggregation-$ijob.Rout" 2> "/home/rstudio/PROJ/rout/aggregation-$ijob.err" &
done


