# Determine longest good-quality sequence by site
library(tidyverse)
library(FluxDataKit)

path <- "~/data/FluxDataKit/v3"

sites <- FluxDataKit::fdk_site_info |>
  filter(!(sitename %in% c("MX-Tes", "US-KS3")))

# determine longest sequence of good-quality data for each site
list_seq <- lapply(sites$sitename, function(site){
  message(sprintf("Analysing %s ----", site))

  # get file name path
  filn <- list.files(
    file.path(path, "fluxnet"),
    pattern = paste0("FLX_", site, ".*_FULLSET_DD.*.csv"),
    recursive = TRUE
  )

  df <- read.csv(file.path(file.path(path, "fluxnet"), filn))

  df_seq <- suppressMessages(
    suppressWarnings(
      try(fdk_get_sequence(
        df,
        site = site,
        qc_threshold = 0.25,
        leng_threshold = 90,
        do_plot = TRUE,
        out_path = file.path(path, "plots")
      )
      )
    )
  )

  if(inherits(df_seq, "try-error")){
    message("!!! plotting failed !!!")
    return(NULL)
  }

  return(df_seq)
})


fdk_site_fullyearsequence <- bind_rows(list_seq)

save(fdk_site_fullyearsequence,
     file = here::here("data/fdk_site_fullyearsequence.rda"),
     compress = "xz"
     )

# df <- readRDS("~/data/FluxDataKit/v3/rsofun_driver_data_v3.rds")
#
# # screen for missing gpp data
# screening <- df |>
#   group_by(sitename) |>
#   unnest(forcing) |>
#   summarize(
#     keep = ifelse(all(is.na(gpp)), FALSE, TRUE)
#   )
#
# df <- left_join(df, screening)
# df <- df |>
#   filter(
#     keep
#   )
#
# # screen manually, drop funky sites
#
# screen <- readODS::read_ods("data/rsofun_sites.ods") |>
#   filter(
#     drop
#   )
#
# df <- df |>
#   filter(
#     !(tolower(sitename) %in% tolower(screen$sitename))
#   ) |>
#   select(
#     -keep
#   )
#
# # screen manually, drop funky years
# screen <- readODS::read_ods("data/rsofun_sites.ods") |>
#   filter(
#     is.na(drop)
#   )
#
# data_fix_years <- df |>
#   filter(tolower(sitename) %in% tolower(screen$sitename)) |>
#   group_by(sitename) |>
#   unnest(forcing) |>
#   left_join(screen) |>
#   mutate(
#     year = as.numeric(format(date, "%Y"))
#   ) |>
#   filter(
#     (year >= start & year <= end)
#   ) |>
#   select(
#     -params_siml,
#     -site_info,
#     -year,
#     -end,
#     -start,
#     -drop,
#     -notes
#   ) |>
#   nest() |>
#   rename(
#     forcing = data
#   ) |>
#   ungroup()
#
# df1 <- df |>
#   filter(tolower(sitename) %in% tolower(screen$sitename)) |>
#   select(
#     -forcing
#   ) |>
#   left_join(data_fix_years)
#
# df2 <- df |>
#   filter(!(tolower(sitename) %in% tolower(screen$sitename)))
#
# data <- bind_rows(df1, df2)
#
# # save data
# saveRDS(data, "data/rsofun_driver_data_clean.rds", compress = "xz")
#
# data |>
#   group_by(sitename) |>
#   do({
#
#     tmp <- .$forcing[[1]] |>
#       tidyr::pivot_longer(
#         col = !contains("date"),
#         names_to = "measurement",
#         values_to = "value"
#       )
#
#     sitename <- .$sitename[1]
#
#     l <- seq(as.Date("1990/1/1"), as.Date("2023/1/1"), "years")
#
#     p <- ggplot(data = tmp) +
#       geom_line(
#         aes(
#           date,
#           value
#         ),
#         colour = "red"
#       ) +
#       geom_vline(xintercept = l) +
#       labs(
#         title = sitename
#       ) +
#       theme_bw() +
#       theme(panel.grid.minor = element_line()
#       ) +
#       facet_grid(
#         measurement ~ .,
#         scales = "free"
#         )
#
#     ggsave(
#       paste0("manuscript/rsofun_input/",sitename,".png"),
#       width = 12,
#       height = 14,
#       dpi = 175
#     )
#
#   })
