library(rvest)

# base html url
url <- "http://dapds00.nci.org.au/thredds/fileServer/ks32/CLEX_Data/PLUMBER2/v1-0/Flux/"

# catalogue
html_page = read_html("https://dap.nci.org.au/thredds/remoteCatalogService?catalog=http://dapds00.nci.org.au/thredds/catalog/ks32/CLEX_Data/PLUMBER2/v1-0/Flux/catalog.xml")

# grab files from the catalogue
files = html_page %>%
  html_nodes("a") %>%
  html_text()

# remove readme and stuff (only retain nc)
files <- files[grepl("*.nc",files)]

# set destination path
dest_path <- "~/data/plumber2"

# loop over all files
lapply(files, function(file){
  download.file(file.path(url, file), file.path(dest_path,file))  
})
