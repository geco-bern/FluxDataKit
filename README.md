# Fluxnet aggregation project

This project is the framework used to create the LEMONTREE "flux data kit", a dataset with consistent model data for use and re-use. In the interest of consistency across the community we re-use the PLUMBER-2 framework, with a few exceptions. The PLUMBER-2 framework generated consistent gap filled data for land surface modelling. We use the same methods (from the underlying FluxnetLSM package), to provide an expanded dataset covering more sites and site years.

The data is generated using [set workflow]() and new releases generated using this workflow when considerable data additions are made to the source (flux) data. Final data will be incrementally deposited in a static [Zenodo repository](https://zenodo.org/record/7258291). Contrary to PLUMBER-2 we do not execute post-hoc data screening. Unless not enough data is available for consistent processing all sites are processed and data is generated. We provide summary statistics on data coverage so users can make an informed decision on how to use the data for their particular use cases.

> DISCLAIMER: Although the this workflow is presented as a functional R package we warn users not to create data themselves. If your required data use the proper released version as deposited on Zenodo. If you do opt to generate data yourselves the authors do not accept any responsibility with respect to the generated results (mistakes and misuse of the package are your own).

## Ecosystem flux data sources

We sourced data from openly available ecosystem flux networks or products, mainly ICOS, OneFlux processed data, the FLUXNET2015 dataset and PLUMBER-2 (which includes various data sources in its own right, see Ukkola et al. 2022). Data was sourced from these locations:

- ICOS data was provided through the ICOS carbon portal, this is a pre-release currently *not publicly available*
- FLUXNET2015 data can be retrieved from the [FLUXNET data portal](https://fluxnet.org/data/fluxnet2015-dataset/)
- OneFlux data can be retrieved from the [Ameriflux data portal](https://ameriflux.lbl.gov/data/download-data/)
- PLUMBER data can be downloaded using [an included script](https://github.com/geco-bern/FluxDataKit/blob/main/data-raw/00_download_plumber_data.R)
- MODIS LAI/FPAR data is downloaded by an included script

Data should be structured in the following directory structure and referenced
to as such in the data generation workflow:

```
data/
   ├─ modis/
   ├─ flux_data/
      ├─ fluxnet2015/
      ├─ icos/
      ├─ oneflux/
      ├─ ameriflux/
```

## Ecosystem flux data selection

Given the various datasets, and at times overlap between the datasets a priority in processing is given to more recent (hopefully) and more complete datasets. In order of processing this means that OneFlux has priority over FLUXNET2015, and Plumber2. ICOS data has priority over FLUXNET2015 for European sites. Overall, Plumber2 mostly fills in the remaining sites in Asia and Australia. The final picking order is thus:

- ICOS
- OneFlux
- FLUXNET2015
- PLUMBER-2

## Data products

### Land Surface Modelling (LSM) data (netCDF)

We deliver gap filled ecosystem flux data in line with the PLUMBER dataset. We refer to the original publication ([Ukkola et al. 2022](https://essd.copernicus.org/articles/14/449/2022/essd-14-449-2022.pdf)) for data details. Data is provided as two netCDF files per site, one file `*Flux.nc` contains all ecosystem fluxes while a `*Met.nc` file contains the matching meteorological values.

#### Exceptions and processing differences

Contrary to the original PLUMBER data we report both data for a closed energy balance, and the raw data inputs (on request of some data users). Furthermore, we report both MODIS based leaf area index (LAI) and fraction of absorbed photosynthetic active radiation (FAPAR). Processing of the MODIS data was also altered and now follows a workflow similar to the one integrated in the {phenocamr} package. Data is smoothed using a LOESS based curve fitting with a BIC optimized smoothing kernel, instead of multiple cubic splines.

### Half-hourly and daily FLUXNET data output (CSV)

To provide easily readable data as requested by some data users we convert the netCDF data to a human-readable CSV file adhering to FLUXNET column- and file-naming conventions. These half-hourly files are further downsampled to a daily time step for modelling efforts which require daily data. The daily data should be easily merged on a day by day basis with remote sensing data as provided by the FluxnetEO data product (Walther et al. 2022).

> Downsampled daily data is an aggregation of the half-hourly data and not, as would be the case when downloading daily data from an ecosystem flux processing chain, a completely separate product. Some discrepancies therefore exist between the downsampled data and the equivalent daily ecosystem flux product.

### p-model drivers (structured R data)

A final data product derived from the initial gap-filled LSM data are p-model driver data for the [`rsofun`](https://github.com/geco-bern/rsofun) package. In the current setup *in-situ* environmental forcing will be combined with GPP values as target data for model calibration.

### Ancillary remote sensing data

For machine learning or other modelling purposes we provide ancillary MODIS based remote sensing data as described in the FluxnetEO dataset. We refer to the original publication and our [FluxnetEO](https://bg.copernicus.org/articles/19/2805/2022/) package for easy reading and processing of the data.

## Data and code availabilty

Data releases are made public via a Zenodo archive at [https://zenodo.org/record/7258291](https://zenodo.org/record/7258291). The processing workflow relies on the `FluxDataKit` R package and a [workflow described in the repository](https://github.com/geco-bern/FluxDataKit/tree/main/analysis). 

## Acknowledgements

The flux data kit is part of the LEMONTREE project and funded by Schmidt Futures and under the umbrella of the Virtual Earth System Research Institute (VESRI).

## References

Ukkola, Anna M., Gab Abramowitz, and Martin G. De Kauwe. "A flux tower dataset tailored for land model evaluation." Earth System Science Data 14.2 (2022): 449-461.

Ukkola, A. M., Haughton, N., De Kauwe, M. G., Abramowitz, G., and Pitman, A. J.: FluxnetLSM R package (v1.0): a community tool for processing FLUXNET data for use in land surface modelling, Geosci. Model Dev., 10, 3379-3390, 2017

Walther, Sophia, et al. "A view from space on global flux towers by MODIS and Landsat: the FluxnetEO data set." Biogeosciences 19.11 (2022): 2805-2840. 
