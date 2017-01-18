#!/bin/bash

rscript="basef.R"

doagg=TRUE
#doagg=FALSE

algo="KD-IC-NML"
#algo="TBATS"


if [[ "$doagg" -eq "TRUE" ]]; then
tag="agg"
  if [[ "$1" -eq 1 ]]; then
  	alliseries=$(seq 1 14 )
  elif [[ "$1" -eq 2 ]]; then
  	alliseries=$(seq 15 28 )
  elif [[ "$1" -eq 3 ]]; then
  	alliseries=$(seq 29 40 )
  elif [[ "$1" -eq 4 ]]; then
  	alliseries=$(seq 41 55 )
  fi
elif [[ "$doagg" -eq "FALSE" ]]; then
tag="bottom"
  if [[ "$1" -eq 1 ]]; then
  	alliseries=$(seq 1 4 1584 )
  elif [[ "$1" -eq 2 ]]; then
  	alliseries=$(seq 2 4 1584 )
  elif [[ "$1" -eq 3 ]]; then
  	alliseries=$(seq 3 4 1584 )
  elif [[ "$1" -eq 4 ]]; then
  	alliseries=$(seq 4 4 1584 )
  fi
fi

echo "${alliseries[@]}"
echo "$tag"

Rscript --vanilla $rscript $algo $doagg ${alliseries[@]} > "/home/rstudio/PROJ/rout/outputFile-$tag-$algo-$1.Rout" 2> "/home/rstudio/PROJ/rout/errorFile-$tag-$algo-$1.Rout"

#for(iseries in seq_along(bottomSeries)){
#	idseries <- bottomSeries[iseries]
#	res_file <- file.path(basef.folder, algo, paste("results_", idseries, "_", algo, ".Rdata", sep = "")) 
#	if(!file.exists(res_file)){
#		print(res_file)
#	}
#}
