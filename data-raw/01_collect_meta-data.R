#!/usr/bin/env Rscript

library(ingestr)
library(icoscp)
library(RCurl)
library(XML)
library(tidyverse)
library(amerifluxr)
source("R/read_plumber.R")

#---- set data paths ----
plumber_path <- "data-raw/flux_data/plumber_fluxnet/"
oneflux_path <- "data-raw/flux_data/oneflux/"
icos_path <- "data-raw/flux_data/icos/"

#--- plumber metatadata ----

# collect Plumber meta-data
if(!file.exists("data-raw/meta-data/plumber_meta-data.rds")){

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
saveRDS(df, file = "data-raw/meta-data/plumber_meta-data.rds", compress = "xz")
}

#---- ameriflux metadata ----

if(!file.exists("data-raw/meta-data/amf_meta-data.rds")){
amf <- amf_site_info()
saveRDS(amf, file = "data-raw/meta-data/amf_meta-data.rds", compress = "xz")
}

#----- oneflux sites ----

if(!file.exists("data-raw/meta-data/oneflux_meta-data.rds")){

of_sites <- unique(substring(list.files(oneflux_path,"*"),5,10))

# list files
files <- list.files(
  oneflux_path,
  utils::glob2rx(paste0("*FULLSET_DD*.csv")),
  full.names = TRUE,
  recursive = TRUE
)

one_flux_sites <- amf_site_info() %>%
 filter(
  SITE_ID %in% of_sites
 )

saveRDS(one_flux_sites, file = "data-raw/meta-data/oneflux_meta-data.rds", compress = "xz")
}

#---- ICOS meta-data ----

icos_list <- icoscp::icos_stations() %>%
  filter(
    theme == "ES"
  )

icos_sites <- unique(substring(list.files(icos_path,"*"),5,10))

icos_list <- icos_list %>%
 filter(id %in% icos_sites)

icos_files <- list.files(
		icos_path,
		glob2rx("*FULLSET_HH*"),
	 	recursive = TRUE,
		full.names = TRUE
		)

years <- lapply(icos_sites, function(site){

	df <- read.table(
		icos_files[grep(site, icos_files)],
		header = TRUE,
		sep = ",")

	year_end <- max(as.numeric(substr(df$TIMESTAMP_START,1,4)))
	year_start <- min(as.numeric(substr(df$TIMESTAMP_START,1,4)))

	return(
	data.frame(
	sitename = site,
	year_start,
	year_end
	))
})

years <- bind_rows(years)
icos_list <- icos_list %>%
	rename(
	'sitename' = 'id'
	) %>%
	left_join(years)

saveRDS(icos_list, file = "data-raw/meta-data/icos_meta-data.rds", compress = "xz")
