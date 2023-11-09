#' climateExtract: Extract Climate Data From a Local or online NETCDF
#' @description A toolbox to ease the extraction and manipulaton of daily gridded climate 
#' data from a NETCDF object available from the European Climate Assessement & Datasets (ECAD) 
#' for specific time period, compute summarized values and link them to set of spatial points.
#' @docType package
#' @name climateExtract
#' @author Reto Schmucki
#' @importFrom methods as
#' @importFrom data.table data.table := melt setDT frollmean frollsum frollapply
#' @importFrom terra rast vect ext writeRaster cellFromXY xyFromCell crds tapp roll extract values
NULL




utils::globalVariables(c("ecad_version"))

#' version of ECA&D to use
#' @format character string (e.g. "22.0")
#' \describe{
#'   \item{ecad_version}{version number used to gather ECAD data}
#' }
"ecad_version"