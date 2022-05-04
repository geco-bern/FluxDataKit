# quality control on the compiled data

library(tidyverse)
library(ggforce)
library(patchwork)

# read data
sites <- readRDS("data/flux_data_kit_site-info.rds")
df <- readRDS("data/p_model_drivers/site_based_drivers.rds")
missing_sites <- sites[which(!c(sites$sitename %in% df$sitename)),]

#----- flatten file -----
df <- df %>%
  filter(
    !is.null(forcing)
  ) %>%
  select(
    sitename,
    forcing
  ) %>%
  unnest(cols = c(forcing))

# calculate site years
site_years <- df %>%
  group_by(sitename) %>%
  summarize(
    time_diff = max(date) - min(date) # time difference in days
  ) %>%
  ungroup() %>%
  summarize( # summarize time in years
    time = round(as.numeric(sum(time_diff)/365))
  )

# message("daily site years:")
# message(site_years)
#
# df <- readRDS("~/Dropbox/tmp/site_based_drivers_HH.rds")
# meta_data <- readRDS("data/flux_data_kit_site-info.rds")
#
# df <- left_join(df, meta_data)
#
# # calculate site years
# site_years <- df %>%
#   group_by(sitename) %>%
#   summarize(
#     time_diff = max(date) - min(date) # time difference in days
#   ) %>%
#   ungroup() %>%
#   summarize( # summarize time in years
#     time = round(as.numeric(sum(time_diff)/365))
#   )
#
# message("HH site years:")
# message(site_years)

#---- plot all GPP time series ----

pdf("~/Desktop/series.pdf", 7, 5)
for (i in unique(df$sitename) ) {

  ss <- df %>%
    filter(sitename == i)

  print(
    ggplot(ss) +
      geom_line(
        aes(
          date,
          gpp
        )
      ) +
      labs(
        title = paste(ss$sitename[1], ss$igbp_land_use[1]),
        subtitle = ss$product[1]
      )
  )
}
dev.off()
