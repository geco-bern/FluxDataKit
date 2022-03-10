#!/bin/bash

# set bundles
#bundles="modis_fpar modis_evi modis_ndiv modis_lst"
bundles="modis_lst_aqua" # modis_lst_terra modis_fpar modis_lai modis_gpp modis_refl_1 modis_refl_2 modis_refl_3 modis_refl_4 modis_refl_5 modis_refl_6 modis_refl_7 modis_refl_8 modis_refl_9 modis_refl_10 modis_refl_11 modis_refl_12 modis_refl_13 modis_refl_14 modis_refl_15 modis_refl_16"

# check dirs
pwd=`pwd`

if [ ${pwd} -ne "flux_data_kit" ]; then
   echo "You are not executing the script in the main project directory, exiting!"
   exit 1
fi

for i in $bundles; do
echo "Submitting bundle $i ..."
bsub -W 72:00 -u $USER -J "format $i" -R "rusage[mem=48000]" "Rscript --vanilla ./data-raw/03_download_GEE.R $i"
done
