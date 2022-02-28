#!/bin/bash

# jobs to run
njobs=10

# check dirs
pwd=`pwd`
base=`basename $pwd`

for ((n=1;n<=${njobs};n++)); do
   echo "Submitting pixel $n ..."
   bsub -W 72:00 -u $USER -J "format cell_${n}" -R "rusage[mem=48000]" "Rscript --vanilla ./data-raw/rscript_format_site_data.R $n ${njobs} FALSE"

done
