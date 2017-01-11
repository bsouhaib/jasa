#!/bin/bash

rscript="evaluation.R"

algo="KD-IC-NML"
doagg=FALSE

Rscript --vanilla $rscript $algo $doagg $1 > "/home/rstudio/PROJ/rout/eval-outputFile-$1.Rout" 2> "/home/rstudio/PROJ/rout/eval-errorFile-$1.Rout"

#Rscript --vanilla evaluation.R "KD-IC-NML" FALSE 1 > "/home/rstudio/PROJ/rout/eval-outputFile-1.Rout" 2> "/home/rstudio/PROJ/rout/eval-errorFile-1.Rout"