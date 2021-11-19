

merge_fluxnet <- function(
  plumber = "/scratch/plumber/"
  icos_etc = "/scratch/CES/ICOS_2021/"
  #icos_drought
) {

  # read in plumber data

  # list plumber sites

  # convert to FLUXNET colnames

  #

  # list files (sites)
  p1_files <- list.files(
    icos_etc,
    utils::glob2rx("*FLUXNET_HH*.csv"),
    recursive = TRUE,
    full.names = TRUE
  )

  # exclude varinfo
  p1_files <- p1_files[!grepl(utils::glob2rx("*VARINFO*"), p1_files)]

  # get site names
  p1_sites <- unlist(lapply(
    strsplit(basename(p1_files),"_"),
    "[[",
    2
  ))

  print(p1_sites)

  #p2_files <- list.files()

  # extract site names from filenames

  # compare overlapping files
  # if found, merge them

  # do full join to merge the two
  # files (which should be structured
  # in the same way)

}

merge_icos()
