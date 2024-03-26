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
# https://zenodo.org/record/7258291

input_path <- "~/data/FluxDataKit/v3/"
tmp_path <- "~/data/FluxDataKit/v3/zenodo_upload/"

#---- purge old data -----

# # remove temporary path
# system(sprintf("rm -rf %s", tmp_path))
#
# # recreate temporary path
# dir.create(tmp_path)
#
# #---- copy new data over ----
# system(
#   sprintf(
#     "cp -R %s/lsm %s/lsm",
#   input_path,
#   tmp_path
#   )
# )
#
# system(
#   sprintf(
#     "cp -R %s/fluxnet %s/fluxnet",
#     input_path,
#     tmp_path
#   )
# )

#---- rename all files in place ----

# rename LSM data

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

# rename FLUXNET data

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
