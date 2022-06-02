data("storms")

qc <- 'wind'

storms <- storms %>%
  # separate into bits
  rowwise() %>%
  mutate(
    qc_bitname = intToBits(wind)[1:8] %>% paste(collapse = "")
  )

storms <- storms %>%
  # separate into bits
  rowwise() %>%
  mutate(
    qc_bitname = intToBits(!!qc)[1:8] %>% paste(collapse = "")
    )

df <- df %>%

  # MODLAND_QC bits
  # 0: Good  quality (main algorithm with or without saturation)
  # 1: Other quality (backup  algorithm or fill values)
  mutate(qc_bit0 = substr( qc_bitname, start=8, stop=8 )) %>%
  mutate(good_quality = ifelse( qc_bit0=="0", TRUE, FALSE )) %>%

  # Sensor
  # 0: Terra
  # 1: Aqua
  mutate(qc_bit1 = substr( qc_bitname, start=7, stop=7 )) %>%
  mutate(terra = ifelse( qc_bit1 == "0", TRUE, FALSE )) %>%

  # Dead detector
  # 0: Detectors apparently  fine  for up  to  50% of  channels  1,  2
  # 1: Dead  detectors caused  >50%  adjacent  detector  retrieval
  mutate(qc_bit2 = substr( qc_bitname, start=6, stop=6 )) %>%
  mutate(dead_detector = ifelse( qc_bit2=="1", TRUE, FALSE )) %>%

  # CloudState
  # 00 0  Significant clouds  NOT present (clear)
  # 01 1  Significant clouds  WERE  present
  # 10 2  Mixed cloud present in  pixel
  # 11 3  Cloud state not defined,  assumed clear
  mutate(qc_bit3 = substr( qc_bitname, start=4, stop=5 )) %>%
  mutate(CloudState = ifelse( qc_bit3=="00", 0,
                              ifelse( qc_bit3=="01",
                                      1, ifelse( qc_bit3=="10", 2, 3 ) ) )) %>%

  # SCF_QC (five level confidence score)
  # 000 0 Main (RT) method used, best result possible (no saturation)
  # 001 1 Main (RT) method used with saturation. Good, very usable
  # 010 2 Main (RT) method failed due to bad geometry, empirical algorithm used
  # 011 3 Main (RT) method failed due to problems other than geometry, empirical algorithm used
  # 100 4 Pixel not produced at all, value couldn???t be retrieved (possible reasons: bad L1B data, unusable MOD09GA data)
  mutate(qc_bit4 = substr( qc_bitname, start=1, stop=3 )) %>%
  mutate(SCF_QC = ifelse(
    qc_bit4=="000",
    0, ifelse( qc_bit4=="001",
               1, ifelse( qc_bit4=="010", 2, ifelse( qc_bit4=="011", 3, 4 ) ) ) )) %>%

  # Actually do the filtering
  mutate(modisvar_filtered = ifelse( CloudState %in% c(0), modisvar_filtered, NA )) %>%
  mutate(modisvar_filtered = ifelse( good_quality, modisvar_filtered, NA )) %>%

  # new addition 5.1.2021
  # mutate(modisvar_filtered = ifelse( !dead_detector, modisvar_filtered, NA )) %>%
  mutate(modisvar_filtered = ifelse( SCF_QC %in% c(0,1), modisvar_filtered, NA ))

library(ingestr)

year_start <- as.numeric(format(min(df$date),"%Y"))
year_end <- as.numeric(format(max(df$date),"%Y"))

# Create daily dataframe

ddf <- init_dates_dataframe( year_start, year_end ) %>%

  # decimal date
  mutate(year_dec = lubridate::decimal_date(date))

# merge N-day dataframe into daily one.
# Warning: here, 'date' must be centered within 4-day period -
# thus not equal to start date but (start date + 2)

ddf <- ddf %>%
  left_join( df, by = "date" )

# LINEAR INTERPOLATION

message("linear ...")
ddf$linear <- stats::approx(
  ddf$year_dec,
  ddf$modisvar_filtered,
  xout=ddf$year_dec )$y

