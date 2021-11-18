#!/usr/bin/env Rscript

source("R/read_nc.R")

# collect Plumber meta-data
path <- "~/data/flux_data_kit/plumber/"

# list files
files <- list.files(
  path,
  utils::glob2rx(paste0("*Flux.nc")),
  full.names = TRUE,
  recursive = TRUE
)

# collect meta data
df <- do.call("rbind",
        lapply(files, function(file){
          read_nc(file, meta_data = TRUE)
        }
    )
  )

# save output to file
saveRDS("data/plumber_meta-data.rds", compress = "xz")
