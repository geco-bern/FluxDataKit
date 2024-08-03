library(tidyverse)

# fdk_out_path = "/data/scratch/jaideep/FluxDataKit/v2023.10.3/"
fdk_out_path = "/data/scratch/jaideep/FluxDataKit/v4.0/"

fig_path = file.path(fdk_out_path, "data_gen_figures")

df_drivers1 = FluxDataKit::fdk_format_drivers_phydro(
  site_info = fdk_site_info |>
    filter(sitename %in% c("AU-ASM", "FR-Pue", "GF-Guy")),
  path = fdk_out_path
  )

df_drivers = readRDS(file = file.path(fdk_out_path, "p_model_drivers.rds"))
df_validation = readRDS(file = file.path(fdk_out_path, "p_model_validation.rds"))

for (i in 1:nrow(df_drivers)){
  site = df_drivers$sitename[[i]]
  # ystart = valid_years %>% filter(Site == site) %>% pull(start_year)
  # yend   = valid_years %>% filter(Site == site) %>% pull(end_year)
  cairo_pdf(filename = paste0(fig_path, "/", site, "_phydro_drivers_new_zoom.pdf"), height=6.4, width=10)
  p5 = df_drivers$forcing_24h[[i]] %>%
    # dplyr::filter(year(date) >= ystart & year(date) <= yend) %>%
    dplyr::select(date, co2, ppfd, netrad, temp, vpd, fapar, rain, ccov) %>%
    pivot_longer(-date) %>%
    dplyr::mutate(type="24-hr mean") %>%
    rbind(df_drivers$forcing_3hrmax[[i]] %>%
            dplyr::select(date, co2, ppfd, netrad, temp, vpd, fapar, rain, ccov) %>%
            pivot_longer(-date) %>%
            dplyr::mutate(type="3-hr maxima")
    ) %>%
    rbind(df_drivers$forcing_daytime[[i]] %>%
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


