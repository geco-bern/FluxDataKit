# Combine MODIS products
library(tidyverse)

# list all sites
sites <- readRDS("data/flux_data_kit_site-info.rds")

# gather data
data <- sites %>%
  group_by(sitename) %>%
  do({

    # get files
    files <- list.files(
      "data-raw/modis/raw/",
      glob2rx(sprintf("%s*.csv",
            .$sitename)),
      full.names = TRUE)

    print(files)
    break

    # loop over files, read and reshuffle
    # into consistent long format
    df <- bind_rows(lapply(files, function(file){
      df <- suppressMessages(read_csv(file))
      df <- df[,c(4:7)]
      df$band <- names(df[,2])
      colnames(df) <- c("date","value","QC","product","band")
      return(df)
    }))
  }) %>%
  nest()

# save all data (compressed)
saveRDS(data, file = "data/modis_data.rds", compress = "xz")
