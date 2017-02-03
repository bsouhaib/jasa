#!/bin/bash

rscript="meancomb.R"

#doagg=TRUE
#tag="agg"
#njobs=11
#nperjobs=5
#njobs=55
#nperjobs=1

doagg=FALSE
tag="bottom"
#njobs=16
#nperjobs=99
njobs=64
nperjobs=25

allijobs=$(seq 1 $njobs )

for ijob in ${allijobs[@]}
do
  start=$(( 0 + ($ijob - 1)* ($nperjobs) + 1 ))
  end=$(( 0 + ($ijob - 1)* ($nperjobs) + ($nperjobs) ))
  alliseries=( $(seq $start $end ) )
  
  echo "${alliseries[@]}"
  
  Rscript --vanilla $rscript $doagg ${alliseries[@]} > "/home/rstudio/PROJ/rout/meancomb-$tag-$ijob.Rout" 2> "/home/rstudio/PROJ/rout/meancomb-$tag-$ijob.err" &

done