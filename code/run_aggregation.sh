#!/bin/bash

rscript="aggregation.R"

njobs=64
nperjobs=4416/$njobs

allijobs=$(seq 1 $njobs )

for ijob in ${allijobs[@]}
do
  start=$(( 0 + ($ijob - 1) * $nperjobs + 1 ))
  end=$(( 0 + ($ijob - 1) * $nperjobs + $nperjobs ))
  allidtest=( $(seq $start $end ) )
  
  echo "${allidtest[@]}"
  Rscript --vanilla $rscript $ijob ${allidtest[@]} > "/home/rstudio/PROJ/rout/aggregation-$ijob.Rout" 2> "/home/rstudio/PROJ/rout/aggregation-$ijob.err" &
done


exit 1
##############

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

#Rscript --vanilla $rscript $1 ${allidtest[@]} > "/home/rstudio/PROJ/rout/aggregation-$1.Rout" 2> "/home/rstudio/PROJ/rout/aggregation-$1.err"

