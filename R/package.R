#' Extract Climate Data From a Local or online NETCDF
#'
#' @description A toolbox to extract and wrangle daily gridded climate data from NETCDF objects available from the European Climate Assessement & Datasets (ECAD) 
#' for specific time period, compute summarized values and link them to set of spatial points.
#'
#' @name climateExtract
#' @author Reto Schmucki
#' @keywords internal
#' @importFrom methods as
#' @importFrom data.table data.table := melt setDT frollmean frollsum frollapply
#' @importFrom terra rast vect ext writeRaster cellFromXY xyFromCell crds tapp roll extract values
"_PACKAGE"

utils::globalVariables(c("ecad_version"))

#' version of ECA&D to use
#' @format character string (e.g. "29.0")
#' \describe{
#'   \item{ecad_version}{version number used to gather ECAD data}
#' }
"ecad_version"