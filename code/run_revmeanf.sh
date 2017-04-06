#!/bin/bash

rscript="revised_meanf.R"

####################
#doagg=TRUE
#tag="agg"
  #njobs=11
  #nperjobs=5
  #njobs=2
  #nperjobs=28
#njobs=55
#nperjobs=1
#njobs=36
#nperjobs=2
####################

doagg=FALSE
tag="bottom"
  #njobs=16
  #nperjobs=99
  #njobs=64
  #nperjobs=25
  #njobs=36
  #nperjobs=44
njobs=?
nperjobs=?

allijobs=$(seq 1 $njobs )

for ijob in ${allijobs[@]}
do
  start=$(( 0 + ($ijob - 1)* ($nperjobs) + 1 ))
  end=$(( 0 + ($ijob - 1)* ($nperjobs) + ($nperjobs) ))
  alliseries=( $(seq $start $end ) )
  
  echo "${alliseries[@]}"
  
  Rscript --vanilla $rscript $doagg ${alliseries[@]} > "/home/rstudio/PROJ/rout/revmeanf-$tag-$ijob.Rout" 2> "/home/rstudio/PROJ/rout/revmeanf-$tag-$ijob.err" &

done
