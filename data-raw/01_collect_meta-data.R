# This script collects all meta-data
# for all available data products
# (note set the correct paths if these do
# not correspond to the defaults as listed
# below)
#

library(ingestr)
library(icoscp)
library(RCurl)
library(XML)
library(dplyr)
library(amerifluxr)

#---- set data paths ----
plumber_path <- "data-raw/flux_data/plumber_fluxnet/"
oneflux_path <- "data-raw/flux_data/oneflux/"
icos_path <- "data-raw/flux_data/icos/"
fluxnet_path <- "data-raw/flux_data/fluxnet2015/"

#--- plumber meta_data ----

# collect Plumber meta_data
if(!file.exists("data-raw/meta_data/plumber_meta-data.rds")){

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
          fdk_convert_lsm(file, meta_data = TRUE)
        }
    )
  )

# save output to file
saveRDS(df, file = "data-raw/meta_data/plumber_meta-data.rds", compress = "xz")
}

#---- ameriflux / fluxnet2015 metadata ----

if(!file.exists("data-raw/meta_data/amf_meta_data.rds")){
amf <- amf_site_info()
saveRDS(amf, file = "data-raw/meta_data/amf_meta-data.rds", compress = "xz")
}

#----- oneflux sites ----

if(!file.exists("data-raw/meta_data/oneflux_meta_data.rds")){

of_sites <- unique(substring(list.files(oneflux_path,"*"),5,10))

# list files
files <- list.files(
  oneflux_path,
  utils::glob2rx(paste0("*FULLSET_HH*.csv")),
  full.names = TRUE,
  recursive = TRUE
)

oneflux_sites <- amf_site_info() %>%
 filter(
  SITE_ID %in% of_sites
 )

saveRDS(oneflux_sites, file = "data-raw/meta_data/oneflux_meta-data.rds", compress = "xz")
}

#---- ICOS meta_data ----
if(!file.exists("data-raw/meta_data/icos_meta_data.rds")){

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

saveRDS(icos_list, file = "data-raw/meta_data/icos_meta-data.rds", compress = "xz")
}

#---- fluxnet2015 meta_data ----

if(!file.exists("data-raw/meta_data/fluxnet_meta_data.rds")){

fluxnet_list <- read_csv("data-raw/meta_data/fluxnet2015_site_list.csv") %>%
  filter(
    license == "CC-BY-4.0"
  ) %>%
  select(-product)

fluxnet_sites <- unique(substring(list.files(fluxnet_path,"*"),5,10))

fluxnet_list <- fluxnet_list %>%
  filter(id %in% fluxnet_sites)

fluxnet_files <- list.files(
  fluxnet_path,
  glob2rx("*FULLSET_DD*"),
  recursive = TRUE,
  full.names = TRUE
)

years <- lapply(fluxnet_sites, function(site){

  df <- read.table(
    fluxnet_files[grep(site, fluxnet_files)],
    header = TRUE,
    sep = ",")

  year_end <- max(as.numeric(substr(df$TIMESTAMP,1,4)))
  year_start <- min(as.numeric(substr(df$TIMESTAMP,1,4)))

  return(
    data.frame(
      sitename = site,
      year_start,
      year_end
    ))
})

years <- bind_rows(years)

fluxnet_list <- fluxnet_list %>%
  rename(
    'sitename' = 'id'
  ) %>%
  left_join(years)

saveRDS(fluxnet_list, file = "data-raw/meta_data/fluxnet-meta_data.rds", compress = "xz")

}

