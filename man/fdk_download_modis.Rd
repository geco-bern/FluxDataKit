% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fdk_download_modis.R
\name{fdk_download_modis}
\alias{fdk_download_modis}
\title{Download MODIS LAI/FPAR}
\usage{
fdk_download_modis(df, path, force = FALSE)
}
\arguments{
\item{df}{data frame with site info}

\item{path}{path where to store the MODIS data}

\item{force}{force a new download even if the data exists in the
output path}
}
\value{
raw MODIS data saved to a desired path
}
\description{
Downloads and smooths MODIS LAI/FPAR values
for merging into the final LSM model data.
Smoothing interpolates values to an half-hourly
time step.
}
\details{
This is an almost verbatim copy of the routine as used to generate
the PLUMBER-2 data. For transparency reasons this part was split out into
a function to provide easier debugging and processing options.
}
