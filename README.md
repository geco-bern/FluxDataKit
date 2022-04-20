# Fluxnet aggregation project

This project is the framework used to create the LEMONTREE flux data kit, a
dataset with consistent model drivers for use and re-use. The data (as of publication)
sources from various ecosystem flux data providers or datasets, most prominently 
these are the FLUXNET2015 dataset, the OneFlux data, an amended version of FLUXNET2015,
ICOS processed data, and Plumber2 data. The latter includes many of the AsiaFlux
and OzFlux sites, in addition to FLUXNET2015.

In order to ensure transparency to the provenance of all data we document the
workflow in various articles.

## Flux data selection

Given the various datasets, and at times overlap between the datasets a priority
in processing is given to more recent (hopefully) and more complete datasets. In
order of processing this means that OneFlux has priority over FLUXNET2015, and 
Plumber2. ICOS data has priority over FLUXNET2015 for European sites. Overall, 
Plumber2 mostly fills in the remaining sites in Asia and Australia. The final
picking order is thus:

- ICOS
- OneFlux
- FLUXNET2015
- Plumber2

### Processing

All data are (currently) aggregated to a daily level to limit the file size and
ease of handling the data. Upon request the data can be processed at the half-hourly
resolution of the source flux data. Data will then be processed on a site-by-site
basis to reduce the final dimensions of the data.

## Ancillary MODIS data

For many modelling efforts ancillary MODIS remote sensing data is required in order
to support these efforts. In this context estimates of FAPAR/LAI come to mind. We
therefore include a selection of common MODIS data products in the flux data kit. We did not interpolate the data, this will remain the prerogative of the user.

## Acknowledgements

The flux data kit is part of the LEMONTREE project and funded by Schmidt Futures and under the umbrella of the Virtual Earth System Research Institute (VESRI).
