#' extract_nc_value_dt
#'
#' This function extract climate data from NETCDF file produced by the European Climate Assesment & Dataset for a specific time-period and available at http://www.ecad.eu/download/ensembles/download.php#datafiles
#' @param firs.year a numeric value defining the first year of the time-period to extract, 1950 if NULL, default=NULL
#' @param last.year a numeric value defining the last year of the time-period to extract, 2014 if NULL, default=NULL
#' @param local_file logical if the .nc data are available on your local disc, if FALSE the data will be downloaded, default=TRUE
#' @param clim_variable string defining the daily climate variable of interest; "mean temp","max temp","min temp","precipitation", default="mean temp"
#' @param statistic string defining the metric to retreave, "mean" or "spread", where the mean is computed across the 100 members and is provided as the "best-guess" fields. The spread is calculated as the difference between the 5th and 95th percentiles over the ensemble to provide a measure indicate of the 90\% uncertainty range. For more details see Cornes et al. (2018) and the guidance on how to use ensemble datasets available from http://surfobs.climate.copernicus.eu/userguidance/use_ensembles.php
#' @param grid_size numeric value in degree defining the resolution of the grid, 0.25 (ca. xx meters) or 0.1 (ca. xx meters), default=0.25
#' @author Reto Schmucki
#' @details This function ask you to select the .nc file containing the data of interest from your local disc, if local_file is FALSE, data will be downloaded from the ECAD. If first.year and last.year are not provided, the function extract the full data set
#' @import ncdf4
#' @import chron
#' @import data.table
#' @export extract_nc_value
#'

#  FUNCTIONS commit test

extract_nc_value_dt <- function(first.year=NULL, last.year=NULL, local_file=TRUE, clim_variable="temp", statistic="mean", grid_size=0.25) {

  if (local_file == TRUE) {

  print("select your climate file [.nc]")
  nc.ncdf <- ncdf4::nc_open(file.choose())

  } else {

  cat(paste0("Let's try to get the ",clim_variable," from ",first.year," at ",grid_size," degree resolution \n"))

    if (clim_variable == "mean temp") {clim_var <- paste0("tg_ens_", statistic)}
    if (clim_variable == "min temp") {clim_var <- paste0("tn_ens_", statistic)}
    if (clim_variable == "max temp") {clim_var <- paste0("tx_ens_", statistic)}
    if (clim_variable == "precipitation") {clim_var <- paste0("rr_ens_", statistic)}

    if (grid_size == 0.25) {grid_size <- "0.25deg"}
    if (grid_size == 0.1) {grid_size <- "0.1deg"}
        urltoget <-paste0("http://www.ecad.eu/download/ensembles/data/Grid_",grid_size,"_reg_ensemble/",clim_var,"_",grid_size,"_reg_v18.0e.nc")

   dest_file <- paste0(clim_var,"_",grid_size,"_reg_v18.0e.nc")

    x <- "N"

    if(file.exists(paste0(clim_var,"_",grid_size,"_reg_v18.0e.nc"))){
        x <- readline("The requested climate data already exist, do you want to download them again? (Y/N) \n")
    	}

    if(!file.exists(paste0(clim_var,"_",grid_size,"_reg_v18.0e.nc")) | x %in% c('Y','y','yes')){
       download.file(urltoget,dest_file)
    }

    cat(paste0("your data (.nc file) is located in ",getwd(),"/",clim_var,"_",grid_size,"_reg_v18.0e.nc \n"))

    nc.ncdf <- ncdf4::nc_open(paste0(clim_var,"_",grid_size,"_reg_v18.0e.nc"))
    }

    # retrieve dimensions
    lon <- ncdf4::ncvar_get(nc.ncdf,"longitude")
    lat <- ncdf4::ncvar_get(nc.ncdf,"latitude")
    nlon <- dim(lon)
    nlat <- dim(lat)
    nc_var <- names(nc.ncdf$var)
    nc_varname <- ncdf4::ncatt_get(nc.ncdf, nc_var,"long_name")$value

    # set day since in the data
    day_since <- ncdf4::ncatt_get(nc.ncdf,"time")$units
    timeserie_length <- length(ncdf4::ncvar_get(nc.ncdf,"time"))
    fillvalue <- ncdf4::ncatt_get(nc.ncdf, nc_var, "_FillValue")
    day_vals <- ncdf4::ncvar_get(nc.ncdf,"time")

    date_seq <- seq(lubridate::ymd(unlist(strsplit(gsub('days since ', '', day_since)," ",))[1]), by = "day",  length.out = timeserie_length)

    dt_iso_dwmy <- data.table::data.table(
                    date = data.table::as.IDate(date_seq),
                    day_vals = day_vals,
                    year = data.table::year(date_seq))

    if (is.null(first.year)) { first.year <- min(dt_iso_dwmy$year)}
    if (is.null(last.year)) { last.year <- max(dt_iso_dwmy$year)}

    firstday <- dt_iso_dwmy[year == first.year, min(day_vals)]
    lastday <- dt_iso_dwmy[year == last.year, max(day_vals)]

    tmp.array <- data.table::data.table(nc_val = c(ncdf4::ncvar_get(nc.ncdf, nc_var, start = c(1, 1, which(day_vals == firstday)), count = c(nlon, nlat, (lastday - firstday) + 1))))

    date_extract <- c(dt_iso_dwmy[day_vals >= firstday & day_vals <= lastday, date])

    result <- list(variable_name = nc_varname,
                  value_array = tmp.array,
                  longitude = lon,
                  latitude = lat,
                  date_extract = date_extract)

  return(result)
}


data_nc <- extract_nc_value_dt(first.year=2012, last.year=2013, local_file=TRUE, clim_variable="temp", statistic="mean", grid_size=0.25)

temporal_mean_dt <- function(data_nc, time_avg=c("annual","monthly","window"), win_length=30) {

  result <- list(longitude = data_nc$longitude, latitude = data_nc$latitude)

  first.year <- min(lubridate::year(data_nc$date_extract))
  last.year <- max(lubridate::year(data_nc$date_extract))

  if ("annual" %in% time_avg){

    annual.mean <- array(NA, c(length(data_nc$longitude), length(data_nc$latitude), (last.year - first.year) + 1))
    year_list <- c()
    for( y in unique(lubridate::year(data_nc$date_extract))) {
      annual.mean[, , y - (first.year - 1)] <- apply(data_nc$value_array[ , , lubridate::year(data_nc$date_extract) == y],
                                                     c(1, 2), mean, na.rm = T)
      year_list <- c(year_list,y)
    }
    annual.mean <- list(value_array=annual.mean,date_extract=year_list,longitude=data_nc$longitude,latitude=data_nc$latitude,variable_name=data_nc$variable_name)
    result <- c(result,annual.mean)
  }

  if ("monthly" %in% time_avg) {

    year_month <- unique(as.numeric(format(data_nc$date_extract, "%Y%m")))
    monthly.mean <- array(NA,c(length(data_nc$longitude),length(data_nc$latitude),length(year_month)))

    for (ym in year_month) {
      monthly.mean[,,which(year_month==ym)] <- apply(data_nc$value_array[,,as.numeric(format(data_nc$date_extract, "%Y%m"))==ym],c(1,2),mean,na.rm=T)
    }
    monthly.mean <- list(value_array=monthly.mean,date_extract=year_month,year_month=data.frame(year=substr(year_month,1,4),month=substr(year_month,5,6))
                         ,longitude=data_nc$longitude,latitude=data_nc$latitude,variable_name=data_nc$variable_name)
    result <- c(result,monthly.mean)
  }


  if ("window" %in% time_avg) {

    roll.mean <- apply(data_nc$value_array[,,],c(1,2),zoo::rollmean,k=win_length,na.rm=T)
    roll.mean <- aperm(roll.mean, c(2,3,1))
    roll.mean <- list(value_array=roll.mean,date_extract=data_nc$date_extract[-c(1:(win_length-1))],longitude=data_nc$longitude,latitude=data_nc$latitude,variable_name=data_nc$variable_name)
    result <- c(result,roll.mean)
    }

  return(result)

}
