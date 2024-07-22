
#' FDK site info file
#'
#' List of sites to processes as well
#' as their meta-data
#'
#' @format DataFrame
#' \describe{
#'   \item{sitename}{Site name}
#'   \item{lon}{Longitude of site}
#'   \item{lat}{Latitude of site}
#'   \item{elv}{elevation}
#'   \item{year_start}{start year of data availability}
#'   \item{year_end}{end year of data availability}
#'   \item{koeppen_code}{Koeppen Geiger code}
#'   \item{igbp_land_use}{IGBP land cover type}
#'   \item{whc}{Total root zone water holding capacity}
#'   \item{product}{Data taken from published product}
#' }
#'

"fdk_site_info"



#' FDK good quality sequences
#'
#' Information on sequences of good quality data for different variables
#'
#' @format DataFrame
#' \describe{
#'   \item{sitename}{Site name}
#'   \item{start_gpp}{Start date of the good quality data sequence for GPP}
#'   \item{year_start_gpp}{Start year of the good quality data sequence for GPP}
#'   \item{year_end_gpp}{End year of the good quality data sequence for GPP}
#'   \item{nyears_gpp}{Number of complete good quality data years for GPP}
#'   \item{drop_gpp}{Whether to drop this site - TRUE if less than one year good-quality data is available}
#' }
#'

"fdk_site_fullyearsequence"
