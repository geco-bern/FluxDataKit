# Latest version (problematic: missing GPP_DT, missing LE-CORR in may sites, weird CO2)
csv_path = "/data_2/FluxDataKit/v3.3/fluxnet/"
out_path = "/data/scratch/jaideep/FluxDataKit/v4.0"

# # Old version (working well so far)
# csv_path = "/data/scratch/jaideep/fluxdata/FLUXDATAKIT_FLUXNET"
# out_path = "/data/scratch/jaideep/FluxDataKit/v2023.10.3"

fig_path = file.path(out_path, "data_gen_figures")
down_path = file.path(out_path, "fluxnet")

dir.create(fig_path, showWarnings = F, recursive = T)
dir.create(down_path, showWarnings = F, recursive = T)

files_csv = list.files(csv_path)

sites = c("AU-ASM", "FR-Pue", "GF-Guy")

for (site in sites){

  # Get filename for HH data for matching site
  file_csv = files_csv[intersect(grep(site, files_csv),
                                 grep("HH", files_csv))]

  message("- reading FLUXNET format halfhourly data")
  hhdf <- readr::read_csv(paste0(csv_path,"/",file_csv))

  hhdf <- hhdf |> dplyr::mutate(TIMESTAMP_START= as.character(TIMESTAMP_START),
                                TIMESTAMP_END= as.character(TIMESTAMP_END))

  # # FDK downsampler
  # # -------------------------------
  # # Legacy FDK downsampler (generally 24-hr means, daytime means for TA and VPD, sum for P)
  # ddf_fdk = FluxDataKit::fdk_downsample_fluxnet(hhdf, site, overwrite = T)#, out_path = out_path)

  # # New FDK downsampler with "legacy" option
  # ddf_legacy_fdk = FluxDataKit::fdk_downsample_fluxnet_phydro(hhdf, site, overwrite = T, save_plots = T, method = "legacy")#, out_path = out_path)

  # # Check that both give identical results
  # library(tidyverse)
  # p = ddf_fdk %>% mutate(DAYLENGTH=NA) %>%
  #   dplyr::select(TIMESTAMP, CO2_F_MDS, DAYLENGTH, FPAR, LAI, LW_IN_F_MDS, NETRAD, P_F, PA_F, SW_IN_F_MDS, TA_DAY_F_MDS, TMAX_F_MDS, TMIN_F_MDS, VPD_DAY_F_MDS, WS_F, GPP_NT_VUT_REF, LE_F_MDS) %>%
  #   rename(TA_F_MDS = TA_DAY_F_MDS,
  #          VPD_F_MDS = VPD_DAY_F_MDS) %>%
  #   pivot_longer(-TIMESTAMP) %>%
  #   mutate(type="1 Legacy_Orig") %>%
  #   bind_rows(
  #     ddf_legacy_fdk %>%
  #       dplyr::select(TIMESTAMP, CO2_F_MDS, DAYLENGTH, FPAR, LAI, LW_IN_F_MDS, NETRAD, P_F, PA_F, SW_IN_F_MDS, TA_F_MDS, TMAX_F_MDS, TMIN_F_MDS, VPD_F_MDS, WS_F, GPP_NT_VUT_REF, LE_F_MDS) %>%
  #       pivot_longer(-TIMESTAMP) %>%
  #       mutate(type="2 Legacy_New")
  #   ) %>%
  #   ggplot(aes(x=TIMESTAMP, y=value, col=type, group=type))+
  #   geom_line(alpha=0.7)+
  #   facet_wrap(~name, scales="free")

  # cairo_pdf(file.path(fig_path, paste0(site,"_check_legacy_drivers.pdf")), width = 9, height = 5)
  # print(
  #   p
  # )
  # dev.off()

  ## Write all 4 kinds of downsampled files
  for (method in c("legacy", "24hr", "3hrmax", "daytime")){
    FluxDataKit::fdk_downsample_fluxnet(hhdf, site, overwrite = T, save_plots = T, method = method, out_path = down_path, fig_path = fig_path)
  }


}
