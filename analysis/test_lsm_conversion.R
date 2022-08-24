source("R/fdk_convert_lsm.R")

test <- fdk_convert_lsm(
  site = "AT-Neu",
  path = "data/tmp/",
  fluxnet_format = TRUE
)

print(head(test))
