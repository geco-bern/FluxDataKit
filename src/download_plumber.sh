#!/bin/bash

# check dirs
pwd=`pwd`

if [ ${pwd} -ne "flux_data_kit"]; then
   echo "You are not executing the script in the main project directory, exiting!"
   exit 1
fi

bsub -W 72:00 -u $USER -J "download plumber" -R "rusage[mem=48000]" "Rscript --vanilla ./data/download_plumber2.R"
