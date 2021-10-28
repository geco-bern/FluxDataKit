#!/bin/bash

# set bundles
bundles="modis_lst modis_fpar modis_evi modis_ndiv"

for i in $bundles; do
echo "Submitting bundle $i ..."
bsub -W 72:00 -u $USER -J "format $i" -R "rusage[mem=48000]" "Rscript --vanilla ./data/download_modis.R $i"
done
