# Fluxnet aggregation project

This project is the framework used to create the LEMONTREE flux data kit, a dataset with consistent model drivers for use and re-use. The formatting of the data follows the requirements of the [rsofun]() package. However, additional fields will be included to expand research into the domains of machine learning.

The data sources from various ecosystem flux data providers or datasets, most prominently these are the FLUXNET2015 dataset, the OneFlux data (an amended version of FLUXNET2015), ICOS processed data, and Plumber2 data. The latter includes many of the AsiaFlux and OzFlux sites, in addition to the FLUXNET2015 dataset.

## Flux data selection

Given the various datasets, and at times overlap between the datasets a priority in processing is given to more recent (hopefully) and more complete datasets. In order of processing this means that OneFlux has priority over FLUXNET2015, and Plumber2. ICOS data has priority over FLUXNET2015 for European sites. Overall, Plumber2 mostly fills in the remaining sites in Asia and Australia. The final picking order is thus:

-   ICOS
-   OneFlux
-   Plumber2 (FLUXNET2015)

All data are (currently) aggregated to a daily level to limit the file size and ease of handling the data. In order to address issues of corrections to meteorological and flux data we use the [FluxnetLSM]() framework and the workflow as described for constructing the [Plumber2 dataset]().

## Workflow & PLUMBER-X

The back-end of the package leverages the FluxnetLSM and FluxnetEO packages to create a workflow which is largely consistent with the code to generate the PLUMBER2 dataset (see exceptions below), while integrating the FluxnetEO dataset to provide ancillary remote sensing data (for machine learning processes). In short, as a side effect of the generation of the p-model driver data one can create land surface model compatible data (in line with the PLUMBER2 dataset).

## Data structure

### PLUMBER-X

As an intermediate step to the generation of the p-model driver data the package creates a dataset in line with the PLUMBER2 land surface modelling dataset. These data aren't necessarily retained (as temporary intermediates), but one can specify to retain these temporary files if they serve a purpose within your workflow. The workflow also allows for rolling releases of PLUMBER-X datasets as soon as new FLUXNET compatible data releases come available. For the goals of the PLUMBER dataset I refert to the original publication ([Ukkola et al. 2022](https://essd.copernicus.org/articles/14/449/2022/essd-14-449-2022.pdf)). The workflow as outlined below (and in the paper) is followed aside from selecting MODIS as the default LAI product (and providing additional FPAR data using the same workflow), and for consistency only global annual CO2 data is used and no site level measurements.

![](https://essd.copernicus.org/articles/14/449/2022/essd-14-449-2022-f01.png)

### p-model drivers

We used the gapfilled and corrected FluxnetLSM data to provide p-model driver data. The required fields include:

| variable | unit             | description                                                          |
|:-----------------------|:-----------------------|:-------------------------------------------------|
| date     | day (YYYY-MM-DD) | date                                                                 |
| temp     | C                | daily mean temperature                                               |
| prec     | mm               | precipitation                                                        |
| vpd      |                  | vapour pressure deficit                                              |
| ppfd     |                  | photosynthetic photon flux density                                   |
| patm     | Pa               | atmospheric pressure                                                 |
| ccov     | %                | cloud cover                                                          |
| ccov_int | %                | cloud cover                                                          |
| snow     | mm               | precipitaton as snow                                                 |
| rain     | mm               | precipitation                                                        |
| fapar    |                  | fraction of photosynthetic active radiation (sourced from FluxnetEO) |
| co2      | ppm              | atmospheric co2 concentration                                        |
| doy      | integer          | Day of Year                                                          |
| tmin     | C                | daily minimum temperature                                            |
| tmax     | C                | daily maximum temperature                                            |

Most of these fields are taken from the *in-situ* ERA-Interim gapfilled FluxnetLSM processed fluxes of the products mentioned above. Sites are not screened for completeness to ensure reasonable coverage. The latter is deferred to the user as this depends on use cases.

### ancillary remote sensing data

For machine learning or other modelling purposes we provide ancillary MODIS based remote sensing data as described in the FluxnetEO dataset. We refer to the original publication and our [FluxnetEO]() package for easy reading and processing of the data.

## Acknowledgements

The flux data kit is part of the LEMONTREE project and funded by Schmidt Futures and under the umbrella of the Virtual Earth System Research Institute (VESRI).
