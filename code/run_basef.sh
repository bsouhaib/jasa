#!/bin/bash

rscript="basef.R"

doagg="TRUE"

#if [[ "$doagg" -eq "TRUE" ]]; then

#tag="agg"
#algo="DYNREG"
#njobs=5
#nperjobs=11

#elif [[ "$doagg" -eq "FALSE" ]]; then

tag="bottom"
algo="KD-IC-NML"
njobs=16
nperjobs=99

#fi


allijobs=$(seq 1 $njobs )

for ijob in ${allijobs[@]}
do
  start=$(( 0 + ($ijob - 1)* ($nperjobs) + 1 ))
  end=$(( 0 + ($ijob - 1)* ($nperjobs) + ($nperjobs) ))
  alliseries=( $(seq $start $end ) )
  
  echo "${alliseries[@]}"
  
  #Rscript --vanilla $rscript $algo $doagg ${alliseries[@]} > "/home/rstudio/PROJ/rout/outputFile-$tag-$algo-$ijob.Rout" 2> "/home/rstudio/PROJ/rout/errorFile-$tag-$algo-$ijob.Rout"

done

