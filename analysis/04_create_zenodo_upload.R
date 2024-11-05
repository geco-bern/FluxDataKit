# This script does the final renaming of all the files
# to a consistent format, as well as zipping them up
# for uploading to Zenodo
#
# This script uses bash system calls
# as well as the "rename" function which
# is not installed by default. Run on linux
# or a bash compatible system.
#
# Compressed data should be uploaded to
# the Zenodo repository:
# https://doi.org/10.5281/zenodo.10885933

input_path <- "/data_2/FluxDataKit/v3.4/"
tmp_path <- "/data_2/FluxDataKit/v3.4/zenodo_upload/"
drivers_filnam <- "rsofun_driver_data_v3.4.rds"
siteinfo_filnam <- "/data_2/FluxDataKit/v3.4/fdk_site_info.csv"
fullyearseq_filnam <- "/data_2/FluxDataKit/v3.4/fdk_site_fullyearsequence.csv"

#---- purge old data -----

# remove temporary path
# system(sprintf("rm -rf %s", tmp_path))

# recreate temporary path
dir.create(tmp_path)
dir.create(paste0(tmp_path, "/fluxnet"))
dir.create(paste0(tmp_path, "/lsm"))

#---- copy new data over ----
# rsofun driver data object
system(
  sprintf(
    "cp %s/%s %s/%s",
    input_path,
    drivers_filnam,
    tmp_path,
    drivers_filnam
  )
)

# site meta info CSV file
system(
  sprintf(
    "cp %s %s/",
    siteinfo_filnam,
    tmp_path
  )
)

# full-year sequence meta info CSV file
system(
  sprintf(
    "cp %s %s/",
    fullyearseq_filnam,
    tmp_path
  )
)

# CSV files
system(
  sprintf(
    "cp -R %s/fluxnet/* %s/fluxnet/",
    input_path,
    tmp_path
  )
)

# NetCDF files
system(
  sprintf(
    "cp -R %s/lsm/* %s/lsm/",
    input_path,
    tmp_path
  )
)


#---- rename all files in place ----

# rename LSM data (NetCDF files)
system(
  sprintf(
    "rename 's/FLUXNET2015/FLUXDATAKIT/g' %s/lsm/*.nc",
    tmp_path
  )
)

system(
  sprintf(
    "rename 's/OxFlux/FLUXDATAKIT/g' %s/lsm/*.nc",
    tmp_path
  )
)

system(
  sprintf(
    "rename 's/LaThuile/FLUXDATAKIT/g' %s/lsm/*.nc",
    tmp_path
  )
)

# rename FLUXNET data (CSV files)
system(
  sprintf(
    "rename 's/PLUMBER/FLUXDATAKIT/g' %s/fluxnet/*.csv",
    tmp_path
  )
)

#---- zip up all data -----

# compress LSM data
system(
  sprintf(
  "
  cd %s/lsm;
  tar -czvf %s/FLUXDATAKIT_LSM.tar.gz *.nc
  ",
  tmp_path,
  tmp_path
  )
)

# compress FLUXNET data
system(
  sprintf(
    "
  cd %s/fluxnet;
  tar -czvf %s/FLUXDATAKIT_FLUXNET.tar.gz *.csv
  ",
    tmp_path,
    tmp_path
  )
)
