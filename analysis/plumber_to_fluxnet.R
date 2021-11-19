source("R/read_plumber.R")
source("R/merge_plumber.R")

df <- merge_plumber(
  path = "~/Desktop/plumber/"
)

colnames(df) <- toupper(colnames(df))
str(df)


fx <- read.csv("~/Desktop/fluxnet/FLX_AR-SLu_FLUXNET2015_SUBSET_HH_2009-2011_1-3.csv",
         sep = ",")

str(fx)
paste(fx$TIMESTAMP_START, fx$TIMESTAMP_END)

# translate variables
# Tair = TA_F
# SWdown = SW_IN_F
# LWdown = LW_IN_F
# Qair =
# VPD = VPD_F
# RH = RH
# Wind = WS_F
# Psurf = PA_F
# CO2air = CO2_F
# Rnet = NETRAD
# Ustar = USTAR
# SWup = SW_OUT
# Qle = LE_F_MDS # CHECK
# Qle_cor = LE_CORR
# Qh = H_F_MDS
# Qh_cor = H_CORR
# Qg = ...
# NEE = # CHECK
# GPP = # CHECK

# output as file or as data frame
# should be able to be dealt with using
# ingestr without too many chances to
# the current workflow
