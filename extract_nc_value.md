#' extract_nc_value
#'
#' Extract climate data from a NETCDF file produced and made available by the 
#' European Climate Assessment & Dataset for a specific time-period and 
#' available at https://surfobs.climate.copernicus.eu/dataaccess/access_eobs.php
#' @param first_year a numeric value defining the first year of the time-period 
#' to extract, 1950 if NULL, default=NULL
#' @param last_year a numeric value defining the last year of the time-period to
#'  extract, 2014 if NULL, default=NULL
#' @param local_file logical if the .nc data are available on your local disc, 
#' if FALSE the data will be downloaded, default=TRUE
#' @param file_path string defining the path of the local file (works only if 
#' local_file = TRUE), default=NULL
#' @param sml_chunk string defining the time period to be downloaded. Chunk 
#' available are "2011-2020", "1995-2010", "1980-1994", "1965-1979", "1950-1964"
#' @param spatial_extent spatial extent to extract data, can be an "sf" or an 
#' "sp" object or a vector with 4 values defining the bounding box c(xmin, ymax,
#'  xmax, ymax), if NULL the entire extent is extracted
#' @param clim_variable string defining the daily climate variable of interest;
#'  "mean temp","max temp","min temp","precipitation", default="mean temp"
#' @param statistic string defining the metric to retrieve, "mean" or "spread",
#'  where the mean is computed across the 100 members and is provided as the
#'  "best-guess" fields.
#' The spread is calculated as the difference between the 5th and 95th
#'  percentiles over the ensemble to provide a measure indicate of the 90\%
#'  uncertainty range. For more details 
#' see Cornes et al. (2018) and the guidance on how to use ensemble datasets
#'  available from 
#' \url{http://surfobs.climate.copernicus.eu/userguidance/use_ensembles.php}
#' @param grid_size numeric value in degree defining the resolution of the grid,
#'  0.25 (ca. 27 kilometres in latitude) or 0.1 (ca. 11 kilometres in latitude),
#'  default=0.25
#' @param ecad_v ECA&D data version, default = package version.
#' @param write_raster logical, if TRUE the output will be written in a
#'  multilayer raster file
#' @param out character string with the filename for the output raster, if null
#'  data will be written in climateExtract_raster.grd 
#' @param return_data logical, if TRUE the data resulting from the extract will
#'  be stored in the object, if false, only the filename of the raster and the
#'  name of the layers are returned in a list, only if write_out is TRUE. 
#' @author Reto Schmucki
#' @details By default, this function ask you to select the .nc file from your
#'  local disc but this can be changed by setting the argument 'local_file' to
#'  FALSE. When local_file is false, the nc file with data will be downloaded
#'  from the ECAD. If first_year and last_year are not provided, the function
#'  extract the full data set. Smaller chunks of about 15 years of the most
#'  recent version of the E-OBS dataset can be specified for download 
#' can be specified directly with the argument "sm_chunk" (period available are
#'  2011-2020, 1995-2010, 1980-1994, 1965-1979, 1950-1964).
#' @return 
#' @export
#'