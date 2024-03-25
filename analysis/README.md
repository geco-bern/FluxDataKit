This directory contains the formal processing routine to generate PLUMBER-X,
or basically data processed according to the PLUMBER-2 `FluxnetLSM` workflow
as described by Ukkola et al.

Scripts need to be run in chronological order

## Scripts

- 00_batch_convert_LSM_data.R - converts FLUXNET data (standard CSV files) to LSM data (NetCDF)
 - the output is land surface model (LSM) compatible data in a netcdf format 
- 01_visualize_fdk_data.R - Plots all converted data for visual checks (not mandatory)
- 03_batch_convert_to_CSV_data.R - converts LSM data to FLUXNET compatible CSVs
 - this data is downsampled to a daily time step
 - the data sticks to FLUXNET formatting
- 04_batch_format_rsofun_drivers.R - converts data to `rsofun` model inputs
 - conversion to ensure compatibility with the `rsofun` package for modelling
