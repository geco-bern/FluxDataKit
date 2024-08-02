library(tidyverse)

# fdk_out_path = "/data/scratch/jaideep/FluxDataKit/v2023.10.3/"
fdk_out_path = "/data/scratch/jaideep/FluxDataKit/v4.0/"

fig_path = file.path(fdk_out_path, "data_gen_figures")

df_drivers = FluxDataKit::fdk_format_drivers_phydro(
  site_info = fdk_site_info |>
    filter(sitename %in% c("AU-ASM", "FR-Pue", "GF-Guy")),
  path = fdk_out_path
  )


for (i in 1:nrow(df_drivers)){
  site = df_drivers$sitename[[i]]
  # ystart = valid_years %>% filter(Site == site) %>% pull(start_year)
  # yend   = valid_years %>% filter(Site == site) %>% pull(end_year)
  cairo_pdf(filename = paste0(fig_path, "/", site, "_phydro_drivers_new.pdf"), height=7.5, width=10)
  p5 = df_drivers$forcing_24h[[i]] %>%
    # dplyr::filter(year(date) >= ystart & year(date) <= yend) %>%
    dplyr::select(date, co2, ppfd, netrad, temp, vpd, fapar, rain, gpp_nt, gpp_dt, le, le_corr, ccov) %>%
    pivot_longer(-date) %>%
    dplyr::mutate(type="24-hr mean") %>%
    rbind(df_drivers$forcing_3hrmax[[i]] %>%
            # dplyr::filter(year(date) >= ystart & year(date) <= yend) %>%
            dplyr::select(date, co2, ppfd, netrad, temp, vpd, fapar, rain, gpp_nt, gpp_dt, le, le_corr, ccov) %>%
            pivot_longer(-date) %>%
            dplyr::mutate(type="3-hr maxima")
    ) %>%
    rbind(df_drivers$forcing_daytime[[i]] %>%
            # dplyr::filter(year(date) >= ystart & year(date) <= yend) %>%
            dplyr::select(date, co2, ppfd, netrad, temp, vpd, fapar, rain, gpp_nt, gpp_dt, le, le_corr, ccov) %>%
            pivot_longer(-date) %>%
            dplyr::mutate(type="daytime means")
    ) %>%
    ggplot(aes(y=value, x=date)) +
    geom_line(aes(group=type, col=type), alpha=0.5) +
    theme_classic() +
    theme(strip.background = element_rect(color = "white", size = 1))+
    facet_wrap(~name, scales = "free")+
    ggtitle(site)
  p5 %>% print()
  dev.off()
}
