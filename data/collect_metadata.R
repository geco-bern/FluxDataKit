#!/usr/bin/env Rscript
library(ingestr)
library(icoscp)
library(RCurl)
library(tidyverse)
library(amerifluxr)

#---- set data paths ----
plumber_path <- "~/data/flux_data_kit/plumber/"
oneflux_path <- "~/data/flux_data_kit/oneflux"
icos_path <- "~/data/flux_data_kit/ICOS/"

#--- plumber metatadata ----

# collect Plumber meta-data

# list files
files <- list.files(
  plumber_path,
  utils::glob2rx(paste0("*Flux.nc")),
  full.names = TRUE,
  recursive = TRUE
)

# collect meta data
df <- do.call("rbind",
        lapply(files, function(file){
          read_plumber(file, meta_data = TRUE)
        }
    )
  )

# save output to file
saveRDS(df, file = "data/plumber_meta-data.rds", compress = "xz")

#---- ameriflux metadata ----

amf <- amf_site_info()

#----- oneflux sites ----

of_sites <- unique(substring(list.files(oneflux_path,"*"),5,10))

# list files
files <- list.files(
  oneflux_path,
  utils::glob2rx(paste0("*FULLSET_DD*.csv")),
  full.names = TRUE,
  recursive = TRUE
)

# collect meta data
of_df <- do.call(
  "rbind",
  lapply(files, function(file){

  })
)


#---- ICOS meta-data ----

icos_list <- icoscp::icos_stations() %>%
  filter(
    theme == "ES"
  )

icos_sites <- unique(substring(list.files(icos_path,"*"),5,10))

# list files
files <- list.files(
  oneflux_path,
  utils::glob2rx(paste0("*FULLSET_DD*.csv")),
  full.names = TRUE,
  recursive = TRUE
)

# collect meta data
icos_df <- do.call(
  "rbind",
  lapply(files, function(file){

  })
)



