utils::globalVariables(c("ecad_version"))

#' version of ECA&D to use
#' @format character string (e.g. "22.0")
#' \describe{
#'   \item{ecad_version}{version number used to gather ECAD data}
#' }
"ecad_version"



#' extract_nc_value
#'
#' Extract climate data from a NETCDF file produced and made available by the 
#' European Climate Assessment & Dataset for a specific period and 
#' available at https://surfobs.climate.copernicus.eu/dataaccess/access_eobs.php
#' @param first_year a numeric value defining the first year of the time period 
#' to extract, 1950 if NULL, default=NULL
#' @param last_year a numeric value defining the last year of the time period to
#'  extract, 2014 if NULL, default=NULL
#' @param local_file logical if the ".nc" data are available on your local disc, 
#' if FALSE, the function will download the data from ECAD data portal,
#'  default=TRUE
#' @param file_path character string with the path to the local ".nc" file. Works
#'  only if local_file = TRUE), default=NULL
#' @param sml_chunk a character string for specific time period to be downloaded. 
#' Chunk available are "2011-2023", "1995-2010", "1980-1994", "1965-1979",
#' and "1950-1964", but check what's available at https://surfobs.climate.copernicus.eu/dataaccess/access_eobs_chunks.php
#' @param spatial_extent object to define the spatial extent for the data 
#' extraction, can be an "sf", "sp" or "SpatialVector" object or a vector with 4 values 
#' defining the bounding box c(xmin, ymax, xmax, ymax). If NULL (default), the
#'  entire extent is extracted
#' @param clim_variable a character string defining the daily climate variable to 
#' retrieve and extract; "mean temp","max temp","min temp","precipitation", 
#' "sea level pressure", "relative humidity", "global radiation", default="mean temp"
#' @param statistic a character string defining the metric to retrieve, "mean" or
#'  "spread", where the mean is computed across 100 members of the ensemble and 
#' is provided as the "best-guess" fields. The spread is calculated as the 
#' difference between the 5th and 95th percentiles over the ensemble to provide
#'  a measure indicative of the 90\% uncertainty range (see details)
#' @param grid_size numeric, measured in degree defining the resolution of the 
#' grid, 0.25 (ca. 27 kilometres in latitude) or 0.1 (ca. 11 kilometres in 
#' latitude), default=0.25
#' @param ecad_v ECA&D data version, default = package version.
#' @param write_raster logical, if TRUE the output will be written in a
#'  multilayer raster file
#' @param out character string with the filename for the output raster, if null
#' data will be written in "climateExtract_raster.tiff". 
#' @param outformat character string with the format for the output raster, 
#' using GDAL shortname. You can use gdal(drivers=TRUE) to see what drivers are 
#' available in your installation; default is set to GEOTiff.
#' @param return_data logical, if TRUE the data resulting from the extract will
#'  be stored in the object, if false, only the filename of the raster and the
#'  name of the layers are returned in a list, only if write_out is TRUE.
#' @param raw_datavals If TRUE, then the actual raw data values from the file are 
#' returned with no conversion to NA (if equal to the missing value/fill value) 
#' or scale/offset applied. Default is TRUE. This reduce the size of the object to 
#' manipulate.
#' @param ... additional arguments for for writing files, see \link[raster]{writeRaster}
#' @details By default, this function asks to select the ".nc" file from your
#'  local disc, but this can be changed by setting the argument 'local_file' to
#'  FALSE. When local_file is false, the nc file with data will be downloaded
#'  from the ECAD. If first_year and last_year are not provided, the function
#'  extract the full data set. Smaller chunks of about 15 years of the most
#'  recent version of the E-OBS dataset can be specified for download 
#' can be specified directly with the argument "sm_chunk" (period available are
#'  2011-2020, 1995-2010, 1980-1994, 1965-1979, 1950-1964). For more details 
#' about the mean and the spread metric see Cornes et al. (2018) and the 
#' guidance on how to use ensemble datasets available from 
#' \url{http://surfobs.climate.copernicus.eu/userguidance/use_ensembles.php}
#' @return If 'write_raster' is TRUE, return a list with the path to raster 
#' Brick written to the disk and the name of the layers in the object (date). If
#' 'return_data' = TRUE, a list of object is returned, with the variable_name,
#' an 3D array with the value extracted, a vector with the longitude, a vector with
#' latitude and a vector with date extracted.
#' @author Reto Schmucki
#' @importFrom methods as
#' @export
#'

extract_nc_value <- function(first_year=NULL, last_year=NULL, local_file=TRUE,
                             file_path=NULL, sml_chunk=NULL, 
                             spatial_extent=NULL, clim_variable="mean temp",
                             statistic="mean", grid_size=0.25, ecad_v = NULL, 
                             write_raster = FALSE, out = NULL, outformat = "GTiff",
                             return_data = TRUE, raw_datavals = TRUE, 
                             ...){

  if (is.null(ecad_v)){ 
    ecad_v = ecad_version
  }

  if (local_file == TRUE) {
      if(is.null(file_path)) {
          message("select your climate file [.nc] or use \"local_file=FALSE\" to 
                 access online data")
          nc.ncdf <- ncdf4::nc_open(file.choose())
      } else {
          nc.ncdf <- ncdf4::nc_open(file_path)
          }
  }else{
  nc.ncdf <- get_nc_online(first_year = first_year, last_year = last_year, 
                           sml_chunk = sml_chunk, clim_variable = clim_variable, 
                           statistic = statistic, grid_size = grid_size, 
                           ecad_v = ecad_v)
  }

  lon <- ncdf4::ncvar_get(nc.ncdf, "longitude")
  lat <- ncdf4::ncvar_get(nc.ncdf, "latitude")
  nc_var <- names(nc.ncdf$var)
  nc_varname <- ncdf4::ncatt_get(nc.ncdf, nc_var, "long_name")$value
  day_since <- ncdf4::ncatt_get(nc.ncdf, "time")$units
  day_vals <- ncdf4::ncvar_get(nc.ncdf, "time")
  fillvalue <- ncdf4::ncatt_get(nc.ncdf, nc_var, "_FillValue")
  offsetvalue <- ncdf4::ncatt_get(nc.ncdf, nc_var, "add_offset")
  scale_factorvalue <- ncdf4::ncatt_get(nc.ncdf, nc_var, "scale_factor")

  scale_factorvalue = ifelse(isTRUE(scale_factorvalue$hasatt), scale_factorvalue$value, 1)
  offsetvalue = ifelse(isTRUE(offsetvalue$hasatt), offsetvalue$value, 0)
  
  date_seq <- as.Date(gsub("days since ", "", day_since, fixed = TRUE), "%Y-%m-%d") + day_vals
  res <- lon[2] - lon[1]

  if(is.null(first_year)){
  start_date <- date_seq[1]
  }else{
  start_date <- as.Date(paste0(first_year, "-01-01"), "%Y-%m-%d")
  }
  if(is.null(last_year)){
  end_date <- rev(date_seq)[1]
  }else{
  end_date <- as.Date(paste0(last_year, "-12-31"), "%Y-%m-%d")
  }
  time_toget <- which(date_seq >= start_date & date_seq <= end_date)
  ext_ <- c()
  if(!is.null(spatial_extent)){
    if(inherits(spatial_extent, "bbox")){
      ext_ <- spatial_extent
    }else{
      if (inherits(spatial_extent, what=c("sf", "SpatialPolygonsDataFrame", 
                                          "SpatialPointsDataFrame", "SpatVect"))){
        ext_ <- sf::st_bbox(terra::vect(spatial_extent))
      }else{
        if(inherits(spatial_extent, "numeric") & length(spatial_extent) == 4){
          if(all(c("xmin", "ymin", "xmax", "ymax") %in% names(spatial_extent))){
            ext_ <- sf::st_bbox(c(xmin = spatial_extent$xmin,
                                  ymin = spatial_extent$ymin,
                                  xmax = spatial_extent$xmax,
                                  ymax = spatial_extent$ymax))
          }else{
            ext_ <- sf::st_bbox(c(xmin = spatial_extent[1],
                                  ymin = spatial_extent[2],
                                  xmax = spatial_extent[3],
                                  ymax = spatial_extent[4]))
          }
        }
      }
      if(!inherits(ext_, "bbox")){
        stop("spatial_extent must be an sf, a spatial or a vector with four 
              values c(xmin, ymin, xmax, ymax)")
      }
    }
    lon_toget <- which(lon >= (ext_$xmin-res) & lon <= (ext_$xmax+res))
    lat_toget <- which(lat >= (ext_$ymin-res) & lat <= (ext_$ymax+res))
  }else{
    lon_toget <- seq_along(lon)
    lat_toget <- seq_along(lat)
  }

  tmp.array <- ncdf4::ncvar_get(nc.ncdf, nc_var, 
                                start = c(lon_toget[1], 
                                          lat_toget[1], 
                                          time_toget[1]), 
                                count = c(length(lon_toget), 
                                          length(lat_toget), 
                                          length(time_toget)),
                                raw_datavals = raw_datavals
                                )
  if(isTRUE(raw_datavals)){
   tmp.array[tmp.array == fillvalue$value] <- NA
  }
  result <- list(variable_name = nc_varname,
                value_array = tmp.array,
                longitude = lon[lon_toget],
                latitude = lat[lat_toget],
                date_extract = date_seq[time_toget],
                scale_factorvalue = scale_factorvalue,
                offsetvalue = offsetvalue,
                write_raster = write_raster,
                raw_datavals = raw_datavals
                )

  if(write_raster == TRUE){
    if(is.null(out)){
      out = "climateExtract_raster.tiff"
    }
    write_to_brick(result, out = out, outformat = outformat, ...)
    message(paste0("writing our output file: ", out))
  }
  if(return_data == FALSE & write_raster == TRUE){
    return(list(rasterFile = out, 
                layersName = result$date_extract))
  }else{
    return(result)
  }
}

#' get_nc_online
#'
#' Download and connect climate data from NetCDF file (.nc) retrieved from
#' \url{https://surfobs.climate.copernicus.eu/dataaccess/access_eobs.php}
#' @param first_year a numeric value defining the first year of the time period 
#' to extract, 1950 if NULL, default=NULL
#' @param last_year a numeric value defining the last year of the time period to
#'  extract, 2014 if NULL, default=NULL
#' @param sml_chunk a character string for specific time period to be downloaded. 
#' Chunk available are "2011-2022", "1995-2010", "1980-1994", "1965-1979",
#' and "1950-1964"
#' @param clim_variable a character string defining the daily climate variable to 
#' retrieve and extract; "mean temp", "max temp", "min temp", "precipitation", 
#' "sea level pressure", "relative humidity", "global radiation", default="mean temp"
#' @param statistic a character string defining the metric to retrieve, "mean" or
#'  "spread", where the mean is computed across 100 members of the ensemble and 
#' is provided as the "best-guess" fields. The spread is calculated as the 
#' difference between the 5th and 95th percentiles over the ensemble to provide
#'  a measure indicative of the 90\% uncertainty range (see details)
#' @param grid_size numeric, measured in degree defining the resolution of the 
#' grid, 0.25 (ca. 27 kilometres in latitude) or 0.1 (ca. 11 kilometres in 
#' latitude), default=0.25
#' @param ecad_v ECA&D data version, default = package version.
#' @details This function access online ECAD data portal to download the climate
#' ".nc" file to local disk. This is an internal function used within the 
#' 'extract_nc_value' function, but can be used independently if needed. 
#' If first_year and last_year are not provided, the function
#'  extract the full data set. Smaller chunks of about 15 years of the most
#'  recent version of the E-OBS dataset can be specified for download 
#' can be specified directly with the argument "sm_chunk" (period available are
#'  2011-2022, 1995-2010, 1980-1994, 1965-1979, 1950-1964). For more details 
#' about the mean and the spread metric see Cornes et al. (2018) and the 
#' guidance on how to use ensemble datasets available from 
#' \url{http://surfobs.climate.copernicus.eu/userguidance/use_ensembles.php}
#' @return A connection via the ncdf4 package to a ".nc" file 
#' downloaded to the local disk
#' @author Reto Schmucki
#' @export
#'

get_nc_online <- function(first_year = first_year, last_year = last_year, 
                          sml_chunk = sml_chunk, clim_variable = clim_variable, 
                          statistic = statistic, grid_size = grid_size, 
                          ecad_v = ecad_v){

  smc <- c("2011-2023", "1995-2010", "1980-1994", "1965-1979", "1950-1964")

  if(is.null(sml_chunk)){
      message(paste0("Try to get the ",clim_variable," from ",first_year," at ",
                grid_size," degree resolution \n"))

    if (clim_variable == "mean temp") {clim_var <- paste0("tg_ens_", statistic)}
    if (clim_variable == "min temp") {clim_var <- paste0("tn_ens_", statistic)}
    if (clim_variable == "max temp") {clim_var <- paste0("tx_ens_", statistic)}
    if (clim_variable == "precipitation") {clim_var <- paste0("rr_ens_", statistic)}
    if (clim_variable == "sea level pressure") {clim_var <- paste0("pp_ens_", statistic)}
    if (clim_variable == "relative humidity") {clim_var <- paste0("hu_ens_", statistic)}
    if (clim_variable == "global radiation") {clim_var <- paste0("qq_ens_", statistic)}

    if (grid_size == 0.25) {grid_size <- "0.25deg"}
    if (grid_size == 0.1) {grid_size <- "0.1deg"}

    if (first_year >= 2011) {
         year_toget <- "2011-2023_"
         urltoget <- paste0("https://knmi-ecad-assets-prd.s3.amazonaws.com/ensembles/data/Grid_",
                             grid_size, "_reg_ensemble/", clim_var, "_",
                             grid_size, "_reg_", year_toget, "v", ecad_v, "e.nc")
         dest_file <- paste0(clim_var,"_",grid_size,"_reg_", year_toget, "v", ecad_v, "e.nc")
     } else {
        urltoget <- paste0("https://knmi-ecad-assets-prd.s3.amazonaws.com/ensembles/data/Grid_",
                           grid_size, "_reg_ensemble/", clim_var, "_",
                           grid_size, "_reg_v", ecad_v,"e.nc")
        dest_file <- paste0(clim_var, "_", grid_size, "_reg_v", ecad_v, "e.nc")
     }
  } else {
    if(!sml_chunk %in% smc){
      stop(paste0("sml_chunk must be one of the following period:\n",smc))
    }else{
      year_toget <- paste0(sml_chunk, "_")
        message(paste0("Try to get the ", clim_variable, " from ", first_year, 
                " at ", grid_size, " degree resolution from sml_chunk data \n"))
    if (clim_variable == "mean temp") {clim_var <- paste0("tg_ens_", statistic)}
    if (clim_variable == "min temp") {clim_var <- paste0("tn_ens_", statistic)}
    if (clim_variable == "max temp") {clim_var <- paste0("tx_ens_", statistic)}
    if (clim_variable == "precipitation") {clim_var <- paste0("rr_ens_", statistic)}
    if (clim_variable == "sea level pressure") {clim_var <- paste0("pp_ens_", statistic)}
    if (clim_variable == "relative humidity") {clim_var <- paste0("hu_ens_", statistic)}
    if (clim_variable == "global radiation") {clim_var <- paste0("qq_ens_", statistic)}

    if (grid_size == 0.25) {grid_size <- "0.25deg"}
    if (grid_size == 0.1) {grid_size <- "0.1deg"}

    urltoget <- paste0("https://knmi-ecad-assets-prd.s3.amazonaws.com/ensembles/data/Grid_",
                        grid_size, "_reg_ensemble/", clim_var, "_", grid_size,
                        "_reg_", year_toget, "v", ecad_v, "e.nc")
    dest_file <- paste0(clim_var, "_", grid_size, "_reg_", year_toget, "v",
                        ecad_v, "e.nc")
    }
  }
    x <- "N"

    if(file.exists(dest_file)){
        message(paste("The requested climate data already exist, we will use ", dest_file, " as input NetCDF"))
    	}

    if(!file.exists(dest_file) | x %in% c('Y','y','yes')){
       utils::download.file(urltoget, dest_file, mode = "wb")
    }
      message(paste0("your data (.nc file) is located in ", getwd(), "/",
               dest_file, "\n"))
    nc.ncdf <- ncdf4::nc_open(dest_file)

  return(nc.ncdf)
}

#' write_to_brick
#'
#' Write the output in a multilayer raster
#' @param x object obtained from the extrat_nc_value function corresponding 
#' to the specific period of interest
#' @param out character string defining the filename to save; default extension 
#' is "climateExtract_raster.tiff".
#' @param outformat character string with the format for the output raster,
#' using GDAL shortname. You can use gdal(drivers=TRUE) to see what drivers are
#' available in your installation; default is set to GEOTiff.
#' @param ... additional arguments for for writing files, see \link[raster]{writeRaster}
#' @details By default, this function overwrites file with the same name if 
#' existing. Layers' names are Date starting with X (e.g. "X2010.01.27")
#' @export
#'
 
write_to_brick <- function(x, out = out, outformat = outformat, ...) {
  a <- x$value_array
  if(isTRUE(x$raw_datavals)){
  a <- (a * x$scale_factorvalue) + x$offsetvalue
  }
  b <- rast(aperm(a[, ncol(a):1,], c(2, 1, 3), resize = TRUE), extent = ext(min(x$longitude), max(x$longitude), min(x$latitude), max(x$latitude)), crs = "epsg:4326")
  names(b) <- as.character(x$date_extract)

  if(!exists("overwrite")){
    overwrite = TRUE
  }
  if(is.null(out)){
    out = "climateExtract_raster.tiff"
  }
   terra::writeRaster(b, filename = out, filetype = ifelse(is.null(outformat), "GTiff", outformat), overwrite = overwrite, ...)
}

#' temporal_aggregate
#'
#' Compute mean climatic values for specific periods from an object obtained 
#' from the extrat_nc_value function
#' @param x object obtained from the extrat_nc_value function or a 
#' rasterBrick with dates as layers
#' @param y point to extract values, must be an sf (sf), a SpatVect (terra) or spatialPointDataFrame 
#' object (sp)
#' @param agg_function function used to aggregate data, "mean" or "sum"
#' @param variable_name character string to identify the resulting variable
#' @param time_step character string defining the level of averaging, "annual",
#' "monthly" or "window"; if time_step = "window" the rolling function is returned,
#' computed over the temporal window defined by "wind_length". time_step = "daily" will
#' return the daily value, per point if points are provided in y.
#' @param win_length integer length of the temporal window to apply the rolling function
#' @param site_col character string with the name of column containing site_id in object 
#' "y". Default is "site_id"
#' @details The output is either a multilayer raster if no points are supplied 
#' in y or a data.table with aggregated statistic computed for each point 
#' provided in y. If points provided fall in areas with no data (e.g. in the sea
#' along the coast), the function will search for the nearest value available, up
#' to 3 cell in each direction. A distance (m) between the point and the raster 
#' cell used to retrieve the climate metric is calculated and documented in the 
#' data output. If the point is within a cell with value, the distance is set to
#' NA.
#' @importFrom methods as
#' @import data.table
#' @author Reto Schmucki
#' @export
#'

temporal_aggregate <- function(x, y = NULL, agg_function = 'mean',
                              variable_name = "average temp", 
                              time_step = c("annual", "monthly", "daily", "window"),
                              win_length = NULL, site_col = "site_id"){

  year = NULL
  month = NULL
  day = NULL
  .SD = NULL
  raw_datavals = FALSE
  site_order = NULL

  if(time_step == "window" & is.null(win_length)){
    win_length <- 30
      message("rolling function will be computed over 30-days window, set win_length
            to change the extent of the window")
  }

  if(!inherits(x, what = c("RasterBrick", "SpatRaster"))){
    if(length(dim(x$value_array)) != 3){
      stop("x must be a rasterBrick, a SpatRaster object or an output from the 
        extrat_nc_value() function")
    }
    raw_datavals <- x$raw_datavals
    scale_factorvalue <- x$scale_factorvalue
    offsetvalue <- x$offsetvalue
    a <- x$value_array
    if(isTRUE(x$raw_datavals)){
    a <- (a * x$scale_factorvalue) + x$offsetvalue
    }
    a <- rast(aperm(a[, ncol(a):1,], c(2, 1, 3), resize = TRUE), extent = ext(min(x$longitude), max(x$longitude), min(x$latitude), max(x$latitude)), crs = "epsg:4326")
    names(a) <- as.character(x$date_extract)
    x <- a
  }
  Date_seq <- as.Date(gsub("X", "", names(x)))
  date_dt <- data.table::data.table(date = Date_seq, 
                                    year = format(Date_seq, "%Y"),
                                    month = format(Date_seq, "%m"),
                                    day = format(Date_seq, "%d"))
    if(time_step == "annual"){
      indices <- as.numeric(as.factor(date_dt$year))
    }
    if(time_step == "monthly"){
      indices <- as.numeric(as.factor(paste0(date_dt$year, ".", date_dt$month)))
    }
  
   if(!is.null(y)){
    if(!inherits(y, what = c("sf", "SpatialPointsDataFrame", "SpatVector"))) {stop("y must be of class 
        'sf', 'SpatialPointsDataFrame' or 'SpatVector'")}
    my_cell <- terra::cellFromXY(x[[1]], crds(terra::vect(y)))
    nona_cell_res <- get_near_nona(x = x, y = y, x_cell = my_cell)
    my_cell <- nona_cell_res$nona_cell
    val <- terra::extract(x[[seq_along(Date_seq)]], my_cell)
    if(site_col %in% names(y)){
      site_id_ <- as.data.frame(y)[, site_col]
    }else{
      site_id_ <- paste0("site_", seq_len(dim(y)[1]))
    }
    dimnames(val)[[1]] <- site_id_

    if(isTRUE(raw_datavals)){
      val <- (t(as.matrix(val)) * scale_factorvalue) + offsetvalue
    } else{
      val <- t(as.matrix(val))
    }

    dt_v <- cbind(date_dt, val)

    if(time_step == "annual"){
      dt_agg <- dt_v[, lapply(.SD, function(x) get(agg_function)(x, na.rm = TRUE)),
                   by = c("year"), .SDcols = site_id_]
      dt_agg <- data.table::melt(dt_agg,
                  id.vars = c("year"),
                  measure.vars = site_id_,
                  variable.name = "site",
                  value.name = c(paste0(agg_function, "_", gsub(" ", "_", variable_name))))
    }
    if(time_step == "monthly"){
      dt_agg <- dt_v[, lapply(.SD, function(x) get(agg_function)(x, na.rm = TRUE)), 
                  by = c("year", "month"), .SDcols = site_id_]
      dt_agg <- data.table::melt(dt_agg,
                  id.vars = c("year", "month"),
                  measure.vars = site_id_,
                  variable.name = "site",
                  value.name = c(paste0(agg_function, "_", gsub(" ", "_", variable_name))))
    }
    if(time_step == "daily"){
      dt_agg <- dt_v[, lapply(.SD, function(x) get(agg_function)(x, na.rm = TRUE)),
                   by = c("year", "month", "day"), .SDcols = site_id_]
      dt_agg <- data.table::melt(dt_agg,
                  id.vars = c("year", "month", "day"),
                  measure.vars = site_id_,
                  variable.name = "site",
                  value.name = c(paste0(agg_function, "_", gsub(" ", "_", variable_name))))
    }
    if(time_step == "window"){
      if(agg_function == 'mean'){
          dt_agg <- data.table::setDT(data.table::frollmean(dt_v[, site_id_, with = FALSE],
                                                            n = win_length,
                                                            fill = NA,
                                                            align = "right",
                                                            na.rm = TRUE))
          names(dt_agg) <- site_id_
          dt_agg <- cbind(dt_v[, -c(site_id_), with = FALSE], dt_agg)
          dt_agg <- data.table::melt(dt_agg,
                      id.vars = c("date", "year", "month", "day"),
                      measure.vars = site_id_,
                      variable.name = "site",
                      value.name = c(paste0("roll", agg_function, "_", 
                                            gsub(" ", "_", variable_name),
                                            win_length,"-d")))
      }
      if(agg_function == 'sum'){
          dt_agg <- data.table::setDT(data.table::frollsum(dt_v[, site_id_, with = FALSE],
                                                            n = win_length,
                                                            fill = NA,
                                                            align = "right",
                                                            na.rm = TRUE))
          names(dt_agg) <- site_id_
          dt_agg <- cbind(dt_v[, -c(site_id_), with = FALSE], dt_agg)
          dt_agg <- data.table::melt(dt_agg,
                      id.vars = c("date", "year", "month", "day"),
                      measure.vars = site_id_,
                      variable.name = "site",
                      value.name = c(paste0("roll", agg_function, "_", 
                                            gsub(" ", "_", variable_name),
                                            win_length,"-d")))
      }
      if(!any(agg_function %in% c("mean", "sum"))){
          dt_agg <- data.table::setDT(data.table::frollapply(dt_v[, site_id_, with = FALSE],
                                                            n = win_length,
                                                            FUN = get(agg_function),
                                                            fill = NA,
                                                            align = "right",
                                                            na.rm = TRUE))
          names(dt_agg) <- site_id_
          dt_agg <- cbind(dt_v[, -c(site_id_), with = FALSE], dt_agg)
          dt_agg <- data.table::melt(dt_agg,
                      id.vars = c("date", "year", "month", "day"),
                      measure.vars = site_id_,
                      variable.name = "site",
                      value.name = c(paste0("roll", agg_function, "_", 
                                            gsub(" ", "_", variable_name),
                                            win_length,"-d")))
      }
    }

    dt_dist <- data.table::data.table(site = site_id_[nona_cell_res[[2]]$gid], 
                    distance_from_pnt = round(nona_cell_res$dist_nona_cell, 0))
    dt_agg <- merge(dt_agg, dt_dist, by = "site", all.x = TRUE)
    s_ord <- data.table::data.table(site=site_id_, site_order= seq_along(site_id_))
    dt_agg <- merge(dt_agg, s_ord, by = "site", all.x = TRUE)[order(site_order), ][, site_order := NULL]
    return(dt_agg)
  }else{
    if(time_step == "window"){
      x_agg <- terra::roll(x, 
                  n = win_length,
                  fun = get(agg_function),
                  type = "to",
                  circular = FALSE,
                  na.rm = FALSE)

      names(x_agg) <- unique(date_dt$date)
    }
    if(any(time_step %in% c("annual", "monthly"))){  
        x_agg <- terra::tapp(as(x, "SpatRaster"), index = indices, fun = agg_function)
      if(time_step == "annual"){
        names(x_agg) <- unique(date_dt$year)
      }
      if(time_step == "monthly"){
        names(x_agg) <- unique(paste0(date_dt$year, ".", date_dt$month))
      }
    }
    if(time_step == "daily"){
      x_agg <- x
      names(x_agg) <- unique(date_dt$date)
    }
    if(isTRUE(raw_datavals)){
      x_agg <- (x_agg * scale_factorvalue) + offsetvalue
    }
  return(x_agg) 
  }
}

#' get_near_nona
#'
#' Return cell number of the nearest data point available in the the raster layer
#' @param x raster object (raster, rasterStack, rasterBrick or SpatRaster)
#' @param y point as an sf, sp or SpatVector object
#' @param x_cell cell id if already extracted, if not provide this will be computed
#' @details This function links a raster cell id to each points and search for 
#' the nearest cell with data (not NA) if the points fall within a cell with NA.
#' @return a list of cell for all points. Point falling in NA cell are assigned 
#' to the nearest with a value within a range of 3 cell around the points. The 
#' sf of points with NA is provided and the distance to the nearest cell with 
#' a non NA value.
#' @importFrom methods as
#' @author Reto Schmucki
#' @export
#'

get_near_nona <- function(x = x, y = y, x_cell = NULL){
    
    if(is.null(x_cell)){
      x_cell <- terra::cellFromXY(x[[1]], crds(terra::vect(y)))
    }
    wna <- which(is.na(terra::extract(x[[1]], x_cell)))
    mc_na <- x_cell[wna]
    if(length(mc_na) > 0){
      near_cell <- c(rep(NA, length(mc_na)))
      dist_cell <- c(rep(NA, length(mc_na)))
      for(i in seq_along(mc_na)){
        a <- mc_na[i]
        row_fact <- c(ncol(x), 2*ncol(x), 3*ncol(x))
        
        ms1 <- c(a + c(-3:3) - row_fact[3],
                a + c(-3:3) - row_fact[2],
                a + c(-3:3) - row_fact[1],
                a + c(-3:3),
                a + c(-3:3) + row_fact[1],
                a + c(-3:3) + row_fact[2],
                a + c(-3:3) + row_fact[3])

        ms2 <- ms1[which(!is.na(terra::extract(x[[1]], c(ms1))))]
        dist_nonan_cell <- sf::st_distance(y[wna[i],], 
                           sf::st_as_sf(data.frame(terra::xyFromCell(x[[1]], 
                                        c(ms2))), 
                                        coords = c("x", "y"), 
                                        crs = 4326))
        near_cell[i] <- ms2[order(dist_nonan_cell)[1]]
        dist_cell[i] <- dist_nonan_cell[order(dist_nonan_cell)[1]]
      }
    x_cell[wna] <- near_cell   
    }
    points_nacell <- y[wna,]
    points_nacell$gid <- wna
  if(length(mc_na) > 0){
    return(list(nona_cell = x_cell, points_nacell, dist_nona_cell = dist_cell))
  }else{
    return(list(nona_cell = x_cell, points_nacell, dist_nona_cell = c(rep(NA, length(mc_na)))))
  }
}