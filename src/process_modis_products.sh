#!/bin/bash

# set bundles
#bundles="modis_fpar modis_evi modis_ndiv modis_lst"
bundles="modis_refl"

# check dirs
pwd=`pwd`

if [ ${pwd} -ne "flux_data_kit" ]; then
   echo "You are not executing the script in the main project directory, exiting!"
   exit 1
fi

for i in $bundles; do
echo "Submitting bundle $i ..."
bsub -W 72:00 -u $USER -J "format $i" -R "rusage[mem=48000]" "Rscript --vanilla ./data/download_modis.R $i"
done
