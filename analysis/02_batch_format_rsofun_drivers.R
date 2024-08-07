# Suppress summarise info
options(tidyverse.quiet = TRUE)
options(dplyr.summarise.inform = FALSE)

library(dplyr)
library(tidyr)
library(ggplot2)
# library(ingestr)
library(rsofun)
# lapply(list.files("R/","*.R", full.names = TRUE), source)

# Path to half-hourly data for downsampling
hh_input_path <- "/data_2/FluxDataKit/v3.3/fluxnet"

# Path to root dir to output downsampled data and figures
out_path <- "/data/scratch/jaideep/FluxDataKit/v4.0/"

# Paths to figures and data
fig_path = file.path(out_path, "data_gen_figures")
dd_path = file.path(out_path, "fluxnet")

dir.create(fig_path, showWarnings = F, recursive = T)
dir.create(dd_path, showWarnings = F, recursive = T)

# half hourly csv files in specified path
files_csv = list.files(hh_input_path)

# read in sites to process
failed_sites <- readRDS(here::here("data/failed_sites.rds"))

sites <- FluxDataKit::fdk_site_info %>%
  filter(!sitename %in% failed_sites)

# sample for testing:
sites <- sites |>
  filter(sitename %in% c("AU-ASM"))#, "FR-Pue", "GF-Guy"))

# # site subset------------------
# # xxx debug
# # chose representative sites for LES book
# use_sites <- c(
#   # "FI-Hyy", # Boreal Forests/Taiga
#   # "US-SRM", # Deserts & Xeric Shrublands
#   # "FR-Pue", # Mediterranean Forests, Woodlands & Scrub
#   # "DE-Hai", # Temperate Broadleaf & Mixed Forests
#   "CH-Oe2"
#   # "US-Tw1", # Temperate Grasslands, Savannas & Shrublands
#   # "AU-How", # Tropical & Subtropical Grasslands, Savannas & Shrubland
#   # "BR-Sa3", # Tropical
#   # "ZM-Mon", # Tropical deciduous forest (xeric woodland)
#   # "US-ICh"  # Tundra
# )
# sites <- sites |>
#   filter(sitename %in% use_sites)
# #----------------------------


# Loop over all sites to process and downsample them
# This part can go into 00_batch_convert_LSM_data.R, but ideally separate downsampling step into its own step
lapply(sites$sitename,
  function(site){

    # Get filename for HH data for matching site
    file_csv = files_csv[intersect(grep(site, files_csv),
                                   grep("HH", files_csv))]

    message("- reading FLUXNET format halfhourly data")
    hhdf <- readr::read_csv(paste0(hh_input_path,"/",file_csv))

    hhdf <- hhdf |> dplyr::mutate(TIMESTAMP_START= as.character(TIMESTAMP_START),
                                  TIMESTAMP_END= as.character(TIMESTAMP_END))

    ## Write all 4 kinds of downsampled files
    for (method in c("legacy", "24hr", "3hrmax", "daytime")){
      output <- try(
        suppressWarnings(
        FluxDataKit::fdk_downsample_fluxnet_phydro(
          hhdf,
          site,
          overwrite = T,
          save_plots = T,
          method = method,
          out_path = dd_path,
          fig_path = fig_path
          )
      )
      )
    }

    if(inherits(output, "try-error")){
      message(paste0("!!! downsampling drivers failed for",site,"|",method,"!!!"))
      return(NULL)
    } else {
      return(output)
    }
  }
)


# loop over all sites and process them to format
# them into the correct rsofun format
driver_data <- lapply(sites$sitename, function(site){

  message(sprintf("Processing %s ----", site))

  message("- compiling drivers")
  # Use a uniform FLUXNET HH input
  # file to generate p-model (rsofun)
  # compatible driver data

 output <-
    try(
    suppressWarnings(
      FluxDataKit::fdk_format_drivers_phydro(
        site_info = sites |> filter(sitename == !!site),
        path = out_path,
        verbose = TRUE
      )
    )
  )

  if(inherits(output, "try-error")){
    message(paste0("!!! formatting drivers failed for",site,"!!!"))
    return(NULL)
  } else {
    return(output)
  }
})

# bind all tibbles into one big tibble
driver_data <- dplyr::bind_rows(driver_data)

# # write all drivers to file
# # apply compression to minimize space
# saveRDS(
#   driver_data,
#   "/data_2/FluxDataKit/v3.3/rsofun_driver_data_v3.3.rds",
#   compress = "xz"
#   )

# remove target variables from forcing dataset
df_drivers_only = driver_data |>
  dplyr::rowwise() |>
  dplyr::mutate(
    dplyr::across(
      .cols = tidyselect::starts_with("forcing"),
      .fns = function(df){
        df |>
          dplyr::select(-tidyselect::starts_with(c("gpp", "le"))) |>
          list()
      }
    )
  )

# collect target variables into validation dataset
df_validation = driver_data |>
  dplyr::select(sitename, forcing_24h) |>
  dplyr::rename(data = forcing_24h) |>
  dplyr::rowwise() |>
  dplyr::mutate(
    dplyr::across(
      .cols = tidyselect::starts_with("data"),
      .fns = function(df){
        df |>
          dplyr::select(date, tidyselect::starts_with(c("gpp", "le"))) |>
          dplyr::mutate(gpp_dt  = ifelse(gpp_qc > 0.5, yes=gpp_dt,  no=NA),
                        gpp_nt  = ifelse(gpp_qc > 0.5, yes=gpp_nt,  no=NA),
                        le_corr = ifelse(le_qc  > 0.5, yes=le_corr, no=NA),
                        le      = ifelse(le_qc  > 0.5, yes=le,      no=NA),
          ) |>
          list()
      }
    )
  )
# |>
#   rename_all(.funs = stringr::str_replace,
#              pattern = "forcing",
#              replacement = "data")

saveRDS(df_drivers_only, file.path(out_path, "p_model_drivers.rds"))
saveRDS(df_validation, file.path(out_path, "p_model_validation.rds"))

saveRDS(driver_data, file.path(out_path, "p_model_combined_drivers_validation.rds"))

### Some Statistics about goodness of data ---------------------------

## Load saved data
df_drivers_only <- readRDS(file.path(out_path, "p_model_drivers.rds"))
df_validation <- readRDS(file.path(out_path, "p_model_validation.rds"))


## Plot combined drivers and validation
for (i in 1:nrow(df_drivers_only)){
  site = df_drivers_only$sitename[[i]]
  # ystart = valid_years %>% filter(Site == site) %>% pull(start_year)
  # yend   = valid_years %>% filter(Site == site) %>% pull(end_year)
  cairo_pdf(filename = paste0(fig_path, "/", site, "_phydro_drivers_new.pdf"), height=6.4, width=10)
  p5 = df_drivers_only$forcing_24h[[i]] %>%
    # dplyr::filter(year(date) >= ystart & year(date) <= yend) %>%
    dplyr::select(date, co2, ppfd, netrad, temp, vpd, fapar, rain, ccov) %>%
    pivot_longer(-date) %>%
    dplyr::mutate(type="24-hr mean") %>%
    rbind(df_drivers_only$forcing_3hrmax[[i]] %>%
            dplyr::select(date, co2, ppfd, netrad, temp, vpd, fapar, rain, ccov) %>%
            pivot_longer(-date) %>%
            dplyr::mutate(type="3-hr maxima")
    ) %>%
    rbind(df_drivers_only$forcing_daytime[[i]] %>%
            dplyr::select(date, co2, ppfd, netrad, temp, vpd, fapar, rain, ccov) %>%
            pivot_longer(-date) %>%
            dplyr::mutate(type="daytime means")
    ) %>%
    rbind(df_validation$data[[i]] %>%
            dplyr::select(date, gpp_nt, gpp_dt, le, le_corr) %>%
            pivot_longer(-date) %>%
            dplyr::mutate(type="obs (24 hr)")
    ) %>%
    # dplyr::filter(year(date) >= 2014 & year(date) <= 2015) %>%
    ggplot(aes(y=value, x=date)) +
    geom_line(aes(group=type, col=type), alpha=0.5) +
    theme_classic() +
    theme(strip.background = element_rect(color = "white", size = 1))+
    facet_wrap(~name, scales = "free")+
    ggtitle(site)
  p5 %>% print()
  dev.off()
}


# Check if all drivers are non-NA and at least 1 full year of data is available in 24-hr dataset
drivers_check_24h = df_drivers_only %>%
  group_by(sitename) %>%
  unnest(forcing_24h) %>%
  summarize(
    across(temp:ccov,
           ~all(!is.na(.))
    ),
    nyears = n()/365
  ) %>%
  pivot_longer(-c(sitename, nyears)) %>%
  group_by(sitename) %>%
  summarize(f_ok = mean(value),
            nyears = mean(nyears)) %>%
  mutate(bad = f_ok != 1 | nyears < 1)

# Check if all drivers are non-NA and at least 1 full year of data is available in acclim dataset
drivers_check_acclim = df_drivers_only %>%
  group_by(sitename) %>%
  unnest(forcing_3hrmax) %>%
  summarize(
    across(temp:ccov,
           ~all(!is.na(.))
    ),
    nyears = n()/365
  ) %>%
  pivot_longer(-c(sitename, nyears)) %>%
  group_by(sitename) %>%
  summarize(f_ok = mean(value),
            nyears = mean(nyears)) %>%
  mutate(bad = f_ok != 1 | nyears < 1)

# Check that contiguity condition is met in both 24h and acclim datasets
drivers_check = drivers_check_24h %>%
  pivot_longer(-sitename) %>%
  mutate(type="24h") %>%
  bind_rows(
    drivers_check_acclim %>%
      pivot_longer(-sitename) %>%
      mutate(type="acclim")
  ) %>%
  pivot_wider(names_from = c(name, type)) %>%
  mutate(bad = bad_24h | bad_acclim)

# Check what combinations of validation data are available
validation_check = df_validation %>%
  group_by(sitename) %>%
  unnest(data) %>%
  summarize(
    across(c(le,le_corr,gpp_dt,gpp_nt),
           c(b = ~any(!is.na(.)), frac = ~mean(!is.na(.)))
            )
    ) %>%
  mutate(lec_dt_nt = le_corr_b & gpp_dt_b & gpp_nt_b,
         lec_dt = le_corr_b & gpp_dt_b & !gpp_nt_b,
         lec_nt = le_corr_b & !gpp_dt_b & gpp_nt_b,
         no_lec = !le_corr_b,
         no_gpp = !gpp_dt_b & !gpp_nt_b
         )


# Summarize number of sites with various combinations of availability of drivers and validation
validation_check %>%
  left_join(drivers_check) %>%
  dplyr::select(-ends_with("frac")) %>%
  summarize(
    total_sites = n(),
    across(-sitename,
    ~sum(.)
    )
  ) %>%
  pivot_longer(everything())

# Write valid sites (sites that satisfy all quality criteria)
validation_check %>%
  left_join(drivers_check) %>%
  filter(!bad) %>%
  filter(lec_nt | lec_dt_nt) %>%
  readr::write_csv(file.path(out_path, "valid_sites_phydro.csv"))


# Visualize data availability
library(rnaturalearth)
library(sf)
library(ggrepel)

world <- ne_countries(scale = "medium", returnclass = "sf")

png(filename = paste0(out_path, "/data_availability_map.png"), height=1000*3, width=2000*3, res=300)
validation_check %>%
  left_join(drivers_check) %>%
  mutate(type = case_when(
    lec_dt_nt ~ "LE_corr + DT + NT",
    lec_dt    ~ "LE_corr + DT",
    lec_nt    ~ "LE_corr + NT",
    no_lec    ~ "no LE_corr",
    no_gpp    ~ "no GPP",
    )
  ) %>%
  mutate(type = ifelse(bad, yes="Bad drivers", no=type)) %>%
  left_join(FluxDataKit::fdk_site_info) %>%
  ggplot() +
  geom_sf(data = world)+
  geom_point(aes(x=lon, y=lat, col=type))+
  geom_text_repel(aes(x=lon, y=lat, label=sitename, col=type), size=2.5, max.overlaps = 30)+
  scale_color_manual(values=c(
    `LE_corr + DT + NT` = "chartreuse3",
    `LE_corr + DT` = "turquoise",
    `LE_corr + NT` = "purple4",
    `no LE_corr` = "orange4",
    `no GPP` = "orange",
    `Bad drivers` = "red"
  ))
dev.off()

