#!/bin/bash

rscript="basef.R"


#if [[ "$doagg" -eq "TRUE" ]]; then

doagg=TRUE
tag="agg"
  #algo="DYNREG"

algo="DETS"
#njobs=36
#nperjobs=2
njobs=55
nperjobs=1

#elif [[ "$doagg" -eq "FALSE" ]]; then

#doagg=FALSE
#tag="bottom"
#algo="KD-IC-NML"

  #njobs=16
  #nperjobs=99
#njobs=64
#nperjobs=25

#fi


allijobs=$(seq 1 $njobs )

for ijob in ${allijobs[@]}
do
  start=$(( 0 + ($ijob - 1)* ($nperjobs) + 1 ))
  end=$(( 0 + ($ijob - 1)* ($nperjobs) + ($nperjobs) ))
  alliseries=( $(seq $start $end ) )
  
  echo "${alliseries[@]}"
  
  Rscript --vanilla $rscript $algo $doagg ${alliseries[@]} > "/home/rstudio/PROJ/rout/outputFile-$tag-$algo-$ijob.Rout" 2> "/home/rstudio/PROJ/rout/errorFile-$tag-$algo-$ijob.Rout" &

done



