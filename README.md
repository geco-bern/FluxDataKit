# Multi-network ecosystem flux data compilation

This project is the framework used to create the LEMONTREE "flux data kit", a dataset with consistent model data for use and re-use. In the interest of consistency across the community we re-use the PLUMBER-2 framework, with a few exceptions. The PLUMBER-2 framework generated consistent gap filled data for land surface modelling. We use the same methods (from the underlying FluxnetLSM package), to provide an expanded dataset covering more sites and site years.

The data is generated using a set workflow and new releases are generated using this workflow when considerable data additions are made available. **The final data products are deposited on [Zenodo](https://doi.org/10.5281/zenodo.10885933).** 

- See [here](https://geco-bern.github.io/FluxDataKit/articles/01_setup.html) for references to original data sources used for creating the FluxDataKit data product.
- See [here](https://geco-bern.github.io/FluxDataKit/articles/02_data_coverage.html) for an overview of data coverage.
- See [here](https://geco-bern.github.io/FluxDataKit/articles/03_data_generation.html) for an overview of steps for creating the data product.
- See [here](https://geco-bern.github.io/FluxDataKit/articles/04_data_use.html) for an example workflow compiling a dataset with complete good-quality data sequences for time series modelling and analysis.

*DISCLAIMER: Although the this workflow is presented as a functional R package we warn users not to create data themselves. If your required data use the proper released version as deposited on Zenodo. If you do opt to generate data yourselves the authors do not accept any responsibility with respect to the generated results (mistakes and misuse of the package are your own).*

## Data sources

We sourced data from openly available ecosystem flux data products:

- PLUMBER-2: https://dx.doi.org/10.25914/5fdb0902607e1. Can be downloaded using [an included script](https://github.com/geco-bern/FluxDataKit/blob/main/data-raw/00_download_plumber_data.R)
- The latest Ameriflux release, downloaded data on 14 Oct 2023 from https://ameriflux.lbl.gov/.
- ICOS Drought2018 release from https://doi.org/10.18160/YVR0-4898.
- ICOS WarmWinter2020 release from https://doi.org/10.18160/2G60-ZHAK.
- MODIS LAI and FPAR data ([MCD15A2H Collection 6.1](https://lpdaac.usgs.gov/products/mcd15a2hv061/), doi:10.5067/MODIS/MCD15A2H.061) 

For generating the data product locally, data from original sources should be structured in the following directory structure and referenced to as such in the data generation workflow:

```
data/
   ├─ modis/
   ├─ cloud_cover/
   ├─ flux_data/
      ├─ plumber/
      ├─ icos_warmwinter2020/
      ├─ icos_drought2018/
      ├─ ameriflux/
```

*DISCLAIMER: The MODIS product MCD15A2H v061 is available only from 2002-07-04 to 2023-02-17. In FluxDataKit, MODIS FPAR/LAI data is extended by a mean seasonal cycle to match the flux data coverage. To retain only original MODIS data from the period covered by the data product, remove data based on the dates on your own.*

## Ecosystem flux data

The flux data source (PLUMBER-2, Ameriflux, ICOS WarmWinter2020, or ICOS Drought2018) is determined for each site based on which source provides the longest data time series. Site meta information is sourced from multiple sources to maximise available information. This is done in scripts `data-raw/01_collect_meta-data.R` and `data-raw/02_compile_final_site_list.R`.

## Data products

### Land Surface Modelling (LSM) data (NetCDF)

We deliver gap filled ecosystem flux data in line with the PLUMBER dataset. We refer to the original publication ([Ukkola et al. 2022](https://essd.copernicus.org/articles/14/449/2022/essd-14-449-2022.pdf)) for data details. Data is provided as two netCDF files per site, one file `*Flux.nc` contains all ecosystem fluxes while a `*Met.nc` file contains the matching meteorological values.

#### Exceptions and processing differences

Contrary to the original PLUMBER data, we report both data for a closed energy balance, and the raw data inputs (on request of some data users). Furthermore, we report both MODIS-based leaf area index (LAI) and fraction of absorbed photosynthetic active radiation (FPAR). Processing of the MODIS data was also altered and now follows a workflow similar to the one integrated in the {phenocamr} package. Data is smoothed using a LOESS based curve fitting with a BIC optimized smoothing kernel, instead of multiple cubic splines.

### Half-hourly and daily FLUXNET data output (CSV)

To provide easily readable data as requested by some data users we convert the NetCDF data to a human-readable CSV file adhering to FLUXNET column- and file-naming conventions. These half-hourly files are further downsampled to a daily time step for modelling efforts which require daily data. The daily data should be easily merged on a day by day basis with remote sensing data as provided by the FluxnetEO data product (Walther et al. 2022).

*Downsampled daily data is an aggregation of the half-hourly data and not, as would be the case when downloading daily data from an ecosystem flux processing chain, a completely separate product. Some discrepancies therefore exist between the downsampled data and the equivalent daily ecosystem flux product.*

### rsofun drivers (structured R data)

A final data product derived from the initial gap-filled LSM data are driver data for the [`rsofun`](https://github.com/geco-bern/rsofun) package. In the current setup, *in-situ* measured model forcing data is combined with GPP and LE values (including their quality-control information) as target data for model calibration.

### Additional data cleaning

Information about the longest sequence of full years (365 days) of good-quality gapfilled daily GPP, LE, and LE_CORR data for each site is provided by package data `fdk_site_fullyearsequence`. This is created by `analysis/03_screen_rsofun_data.R`. It provides information about the start and end date and the full years for which sequences are available.

### Ancillary remote sensing data

For machine learning or other modelling purposes, we provide ancillary MODIS based remote sensing data as described in the FluxnetEO dataset. We refer to the original publication and our [FluxnetEO](https://bg.copernicus.org/articles/19/2805/2022/) package for easy reading and processing of the data.

## Data and code availabilty

Data releases are made public via a Zenodo archive at [https://doi.org/10.5281/zenodo.10885933](https://doi.org/10.5281/zenodo.10885933). The processing workflow relies on the `FluxDataKit` R package and a [workflow described in the repository](https://github.com/geco-bern/FluxDataKit/tree/main/analysis). 

## Acknowledgements

The flux data kit is part of the LEMONTREE project and funded by Schmidt Futures and under the umbrella of the Virtual Earth System Research Institute (VESRI).

## References

Ukkola, Anna M., Gab Abramowitz, and Martin G. De Kauwe. "A flux tower dataset tailored for land model evaluation." Earth System Science Data 14.2 (2022): 449-461.

Ukkola, A. M., Haughton, N., De Kauwe, M. G., Abramowitz, G., and Pitman, A. J.: FluxnetLSM R package (v1.0): a community tool for processing FLUXNET data for use in land surface modelling, Geosci. Model Dev., 10, 3379-3390, 2017

Walther, Sophia, et al. "A view from space on global flux towers by MODIS and Landsat: the FluxnetEO data set." Biogeosciences 19.11 (2022): 2805-2840. 
