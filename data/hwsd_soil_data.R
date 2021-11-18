# amend soil data, using the HWSD

library(hwsdr)
library(tidyverse)

df <- readRDS("data/plumber_meta-data.rds")

# grab HWSD data
df <- df %>%
  rowwise() %>%
  mutate(
    sand = hwsdr::ws_subset(
      site = sitename,
      location = c(longitude, latitude),
      param = "ALL"
    )
  )

