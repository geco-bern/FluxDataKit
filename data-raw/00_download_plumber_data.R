# This routine downloads the PLUMBER-2 data
# automatically, this does not require any login
# information

# load libraries
library(rvest)

# set destination path
# dest_path <- "/data/scratch/FDK_inputs/flux_data/plumber/"
dest_path <- "~/data/FluxDataKit/FDK_inputs/plumber/"

# set catalogues
catalogues <- data.frame(
  catalogue = c(
  "https://dap.nci.org.au/thredds/remoteCatalogService?catalog=http://dapds00.nci.org.au/thredds/catalog/ks32/CLEX_Data/PLUMBER2/v1-0/Flux/catalog.xml",
  "https://dap.nci.org.au/thredds/remoteCatalogService?catalog=http://dapds00.nci.org.au/thredds/catalog/ks32/CLEX_Data/PLUMBER2/v1-0/Met/catalog.xml"
  ),
  url = c(
    "http://dapds00.nci.org.au/thredds/fileServer/ks32/CLEX_Data/PLUMBER2/v1-0/Flux/",
    "http://dapds00.nci.org.au/thredds/fileServer/ks32/CLEX_Data/PLUMBER2/v1-0/Met/"
    )
)

# loop over catalogues and files
apply(catalogues, 1, function(cat){

  # set url
  url <- cat['url']
  catalogue <- cat['catalogue']

  # catalogue
  html_page <- read_html(catalogue)

  # grab files from the catalogue
  files <- html_page |>
    html_nodes("a") |>
    html_text()

  # remove readme and stuff (only retain nc)
  files <- files[grepl("*.nc",files)]

  # loop over all files
  lapply(files, function(file){

    # download data
    try(
      download.file(
        file.path(url, file),
        file.path(dest_path,file)
        )
      )

    # don't return anything
    return(invisible())
  })

  # don't return anything
  return(invisible())
})
