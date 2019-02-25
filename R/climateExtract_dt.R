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
#' @import data.table
#' @import lubridate
#' @import matrixStats
#' @import RccpRoll
#' @export extract_nc_value
#'

#  FUNCTIONS commit test

extract_nc_value_dt <- function(first.year=NULL, last.year=NULL, local_file=TRUE, clim_variable="mean temp", statistic="mean", grid_size=0.25) {

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
       download.file(urltoget, dest_file, mode="wb")
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

    date_seq <- seq(lubridate::ymd(unlist(strsplit(gsub('days since ', '', day_since)," ",))[1]),
                                    by = "day",  length.out = timeserie_length)

    dt_iso_dwmy <- data.table::data.table(
                    date = data.table::as.IDate(date_seq),
                    day_vals = day_vals,
                    year = data.table::year(date_seq),
                    month = data.table::month(date_seq),
                    day = data.table::mday(date_seq),
                    week = data.table::isoweek(date_seq),
                    week_day = c(7,1:6)[data.table::wday(date_seq)])

    if (is.null(first.year)) { first.year <- min(dt_iso_dwmy$year)}
    if (is.null(last.year)) { last.year <- max(dt_iso_dwmy$year)}

    firstday <- dt_iso_dwmy[year == first.year, min(day_vals)]
    lastday <- dt_iso_dwmy[year == last.year, max(day_vals)]

    tmp.array <- ncdf4::ncvar_get(nc.ncdf, nc_var, start = c(1, 1, which(day_vals == firstday)),
                                  count = c(nlon, nlat, (lastday - firstday) + 1))

    sub_dt_iso_dwmy <- c(dt_iso_dwmy[day_vals >= firstday & day_vals <= lastday, ])

    result <- list(variable_name = nc_varname,
                  value_array = tmp.array,
                  longitude = lon,
                  latitude = lat,
                  dt_iso_dwmy = dt_iso_dwmy[day_vals >= firstday & day_vals <= lastday, ][ , indx := seq_len(.N)])

  return(result)
}

#' temporal_mean
#'
#' This function compute mean climatic value for specific periods "annual", "monthly", or using a sliding "window" of specific length, from an object obtained from the extrat_nc_value() function
#' @param data_nc object obtained from the extrat_nc_value() function corresponding to the time-period of interest
#' @param time_val character string defining the level of averaging, "annual", "monthly", "weekly", "seasonal", "roll_window"
#' @param window_n the number of days defining the span of the sliding window, default set to 30 days
#' @param season_month first month defining the seasonal period of interest, default=NULL
#' @param season_n the number of days defining the seasonal period of interest, default=NULL'
#' @author Reto Schmucki
#' @details This function can be relatively slow if you compute sliding window average over a long period
#' @import data.table
#' @import lubridate
#' @import matrixStats
#' @import RccpRoll
#' @export temporal_mean
#'

temporal_mean_dt <- function(data_nc, time_val=c("annual", "monthly", "weekly", "season", "roll_window"), window_n=30,
                              season_month=NULL, season_n=NULL) {

  result <- list(longitude = data_nc$longitude, latitude = data_nc$latitude)

  first.year <- min(lubridate::year(data_nc$dt_iso_dwmy$date))
  last.year <- max(lubridate::year(data_nc$dt_iso_dwmy$date))

  if("annual" %in% time_val){

    annual.mean <- array(NA, c(length(data_nc$longitude), length(data_nc$latitude), (last.year - first.year) + 1))
    year_list <- c()
    for( y in unique(lubridate::year(data_nc$dt_iso_dwmy$date))) {

      annual.mean[, , y - (first.year - 1)] <- apply(data_nc$value_array[ , , data_nc$dt_iso_dwmy$year == y],
                                                     c(1, 2), matrixStats::mean2, na.rm = T)
      year_list <- c(year_list, y)
    }

    annual.mean <- list(metric_name = "mean.annual",
                        value_array = annual.mean,
                        date_extract = year_list,
                        longitude = data_nc$longitude,
                        latitude = data_nc$latitude,
                        variable_name = data_nc$variable_name)

    result <- list(result, annual.mean)
  }

  if("monthly" %in% time_val) {
    year_month <- unique(data_nc$dt_iso_dwmy[, .(year, month)])
    monthly.mean <- array(NA, c(length(data_nc$longitude), length(data_nc$latitude), year_month[, .N]))

    for(ym in seq_len(year_month[, .N])) {

      monthly.mean[ , , ym] <- apply(data_nc$value_array[, , data_nc$dt_iso_dwmy[year == year_month[ym, year] &
                                      month == year_month[ym, month], indx]], c(1, 2), matrixStats::mean2, na.rm = T)
    }

    monthly.mean <- list(metric_name = "mean.monthly",
                         value_array = monthly.mean,
                         date_extract = year_month,
                         longitude = data_nc$longitude,
                         latitude = data_nc$latitude,
                         variable_name = data_nc$variable_name)

    result <- list(result, monthly.mean)
  }

  if("weekly" %in% time_val) {
    year_month_week <- unique(data_nc$dt_iso_dwmy[, .(year, month, week)])
    weekly.mean <- array(NA, c(length(data_nc$longitude), length(data_nc$latitude), year_month_week[, .N]))

    for (ymw in seq_len(year_month_week[, .N])) {

      weekly.mean[ , , ymw] <- apply(data_nc$value_array[, , data_nc$dt_iso_dwmy[year == year_month_week[ymw, year] &
                                      month == year_month_week[ymw, month] & week == year_month_week[ymw, week], indx]],
                                      c(1, 2), matrixStats::mean2, na.rm = T)
    }

    weekly.mean <-   list(metric_name = "mean.weekly",
                          value_array = weekly.mean,
                          date_extract = year_month_week,
                          longitude = data_nc$longitude,
                          latitude = data_nc$latitude,
                          variable_name = data_nc$variable_name)

    result <- list(result, weekly.mean)
  }

  if("season" %in% time_val) {

    if(is.null(season_month)){cat("You need to define the season_month argument")
                              stop()}

    if(is.null(season_month)){cat("You need to define the season_n argument")
                             stop()}

    year_season <- unique(data_nc$dt_iso_dwmy[month == season_month, .(year)])
    season.mean <- array(NA, c(length(data_nc$longitude), length(data_nc$latitude), year_season[, .N]))
    season_date <- data.table::data.table()
    for (ys in seq_len(year_season[, .N])) {

      start_indx <- min(data_nc$dt_iso_dwmy[year == year_season[ys, year] & month == season_month, indx])

      if(data_nc$dt_iso_dwmy[,.N] < c(start_indx + season_n)){
        season_date <- rbind(season_date,data.table::data.table(data_nc$dt_iso_dwmy[start_indx, date],
                                                      data_nc$dt_iso_dwmy[start_indx, date]+season_n,
                                                      season_n))
        next()
        }

      season_date <- rbind(season_date,data.table::data.table(data_nc$dt_iso_dwmy[start_indx, date],
                                                    data_nc$dt_iso_dwmy[c(start_indx + season_n), date],
                                                    season_n))

      season.mean[ , , ys] <- apply(data_nc$value_array[, , c(start_indx:(start_indx + season_n))],
                                      c(1, 2), matrixStats::mean2, na.rm = T)
    }

    names(season_date) <- c("season_start_date", "season_end_date", "season_length")

    season.mean <-   list(metric_name = "mean.season",
                          value_array = season.mean,
                          date_extract = season_date,
                          longitude = data_nc$longitude,
                          latitude = data_nc$latitude,
                          variable_name = data_nc$variable_name)

    result <- list(result, season.mean)
  }

  if("roll_window" %in% time_val) {

    roll.mean <- apply(data_nc$value_array[ , , ], c(1, 2), RcppRoll::roll_mean, n = window_n, na.rm = T)
    roll.mean <- aperm(roll.mean, c(2, 3, 1))

    roll.mean <- list(metric_name = "mean.rolling_window",
                      value_array = roll.mean,
                      date_extract = data_nc$dt_iso_dwmy[-c(1:(window_n - 1)), .(date)],
                      longitude = data_nc$longitude,
                      latitude = data_nc$latitude,
                      variable_name = data_nc$variable_name)

    result <- list(result, roll.mean)
    }

  return(result)
}

#' temporal_sum_dt
#'
#' This function compute sum of climatic value for specific periods "annual", "monthly", "weekly", "seasonal" or using a sliding "window" of specific length, from and object obtained from the extrat_nc_value() function
#' @param data_nc object obtained from the extrat_nc_value() function corresponding to the time-period of interest
#' @param time_val character string defining the level of accummulation, "annual", "monthly", "weekly", "seasonal", "roll_window"
#' @param window_n the number of days defining the span of the sliding window, default set to 30 days
#' @param season_month first month defining the seasonal period of interest, default=NULL
#' @param season_n the number of days defining the seasonal period of interest, default=NULL'
#' @author Reto Schmucki
#' @details This function can be relatively slow if you compute sliding window average over a long period
#' @import data.table
#' @import lubridate
#' @import matrixStats
#' @import RccpRoll
#' @export temporal_sum_dt
#'

temporal_sum_dt <- function(data_nc, time_val=c("annual", "monthly", "weekly", "season", "roll_window"), window_n=30,
                              season_month=NULL, season_n=NULL) {

  result <- list(longitude = data_nc$longitude, latitude = data_nc$latitude)

  first.year <- min(lubridate::year(data_nc$dt_iso_dwmy$date))
  last.year <- max(lubridate::year(data_nc$dt_iso_dwmy$date))

  if("annual" %in% time_val){

    annual.sum <- array(NA, c(length(data_nc$longitude), length(data_nc$latitude), (last.year - first.year) + 1))
    year_list <- c()
    for( y in unique(lubridate::year(data_nc$dt_iso_dwmy$date))) {

      annual.sum[, , y - (first.year - 1)] <- apply(data_nc$value_array[ , , data_nc$dt_iso_dwmy$year == y],
                                                     c(1, 2), sum, na.rm = T)
      year_list <- c(year_list, y)
    }

    annual.sum <- list( metric_name = "sum.annual",
                        value_array = annual.sum,
                        date_extract = year_list,
                        longitude = data_nc$longitude,
                        latitude = data_nc$latitude,
                        variable_name = data_nc$variable_name)

    result <- list(result, annual.sum)
  }

  if("monthly" %in% time_val) {
    year_month <- unique(data_nc$dt_iso_dwmy[, .(year, month)])
    monthly.sum <- array(NA, c(length(data_nc$longitude), length(data_nc$latitude), year_month[, .N]))

    for(ym in seq_len(year_month[, .N])) {

      monthly.sum[ , , ym] <- apply(data_nc$value_array[, , data_nc$dt_iso_dwmy[year == year_month[ym, year] &
                                      month == year_month[ym, month], indx]], c(1, 2), sum, na.rm = T)
    }

    monthly.sum <- list( metric_name = "sum.monthly",
                         value_array = monthly.sum,
                         date_extract = year_month,
                         longitude = data_nc$longitude,
                         latitude = data_nc$latitude,
                         variable_name = data_nc$variable_name)

    result <- list(result, monthly.sum)
  }

  if("weekly" %in% time_val) {
    year_month_week <- unique(data_nc$dt_iso_dwmy[, .(year, month, week)])
    weekly.sum <- array(NA, c(length(data_nc$longitude), length(data_nc$latitude), year_month_week[, .N]))

    for (ymw in seq_len(year_month_week[, .N])) {

      weekly.sum[ , , ymw] <- apply(data_nc$value_array[, , data_nc$dt_iso_dwmy[year == year_month_week[ymw, year] &
                                      month == year_month_week[ymw, month] & week == year_month_week[ymw, week], indx]],
                                      c(1, 2), sum, na.rm = T)
    }

    weekly.sum <-   list(metric_name = "sum.weekly",
                         value_array = weekly.sum,
                         date_extract = year_month_week,
                         longitude = data_nc$longitude,
                         latitude = data_nc$latitude,
                         variable_name = data_nc$variable_name)

    result <- list(result, weekly.sum)
  }

  if("season" %in% time_val) {

    if(is.null(season_month)){cat("You need to define the season_month argument")
                              stop()}

    if(is.null(season_month)){cat("You need to define the season_n argument")
                             stop()}

    year_season <- unique(data_nc$dt_iso_dwmy[month == season_month, .(year)])
    season.sum <- array(NA, c(length(data_nc$longitude), length(data_nc$latitude), year_season[, .N]))
    season_date <- data.table::data.table()
    for (ys in seq_len(year_season[, .N])) {

      start_indx <- min(data_nc$dt_iso_dwmy[year == year_season[ys, year] & month == season_month, indx])

      if(data_nc$dt_iso_dwmy[,.N] < c(start_indx + season_n)){
        season_date <- rbind(season_date,data.table::data.table(data_nc$dt_iso_dwmy[start_indx, date],
                                                      data_nc$dt_iso_dwmy[start_indx, date]+season_n,
                                                      season_n))
        next()
        }

      season_date <- rbind(season_date, data.table::data.table(data_nc$dt_iso_dwmy[start_indx, date],
                                                    data_nc$dt_iso_dwmy[c(start_indx + season_n), date],
                                                    season_n))

      season.sum[ , , ys] <- apply(data_nc$value_array[, , c(start_indx:(start_indx + season_n))],
                                      c(1, 2), sum, na.rm = T)
    }

    names(season_date) <- c("season_start_date", "season_end_date", "season_length")

    season.sum <-   list(metric_name = "sum.season",
                         value_array = season.sum,
                         date_extract = season_date,
                         longitude = data_nc$longitude,
                         latitude = data_nc$latitude,
                         variable_name = data_nc$variable_name)

    result <- list(result, season.sum)
  }

  if("roll_window" %in% time_val) {

    roll.sum <- apply(data_nc$value_array[ , , ], c(1, 2), RcppRoll::roll_sum, n = window_n, na.rm = T)
    roll.sum <- aperm(roll.sum, c(2, 3, 1))

    roll.sum <- list( metric_name = "sum.rolling_window",
                      value_array = roll.sum,
                      date_extract = data_nc$dt_iso_dwmy[-c(1:(window_n - 1)), .(date)],
                      longitude = data_nc$longitude,
                      latitude = data_nc$latitude,
                      variable_name = data_nc$variable_name)

    result <- list(result, roll.sum)
    }

  return(result)
}

#' temporal_sd_dt
#'
#' This function compute standard deviation of climatic value for specific periods "annual", "monthly", "weekly", "seasonal" or using a sliding "window" of specific length, from and object obtained from the extrat_nc_value() function
#' @param data_nc object obtained from the extrat_nc_value() function corresponding to the time-period of interest
#' @param time_val character string defining the level standard deviation should be computed, "annual", "monthly", "weekly", "seasonal", "roll_window"
#' @param window_n the number of days defining the span of the sliding window, default set to 30 days
#' @param season_month first month defining the seasonal period of interest, default=NULL
#' @param season_n the number of days defining the seasonal period of interest, default=NULL'
#' @author Reto Schmucki
#' @details This function can be relatively slow if you compute sliding window average over a long period
#' @import data.table
#' @import lubridate
#' @import matrixStats
#' @import RccpRoll
#' @export temporal_sd_dt
#'

temporal_sd_dt <- function(data_nc, time_val=c("annual", "monthly", "weekly", "season", "roll_window"), window_n=30,
                              season_month=NULL, season_n=NULL) {

  result <- list(longitude = data_nc$longitude, latitude = data_nc$latitude)

  first.year <- min(lubridate::year(data_nc$dt_iso_dwmy$date))
  last.year <- max(lubridate::year(data_nc$dt_iso_dwmy$date))

  if("annual" %in% time_val){

    annual.sd <- array(NA, c(length(data_nc$longitude), length(data_nc$latitude), (last.year - first.year) + 1))
    year_list <- c()
    for( y in unique(lubridate::year(data_nc$dt_iso_dwmy$date))) {

      annual.sd[, , y - (first.year - 1)] <- apply(data_nc$value_array[ , , data_nc$dt_iso_dwmy$year == y],
                                                     c(1, 2), sd, na.rm = T)
      year_list <- c(year_list, y)
    }

    annual.sd <- list(  metric_name = "sd.annual",
                        value_array = annual.sd,
                        date_extract = year_list,
                        longitude = data_nc$longitude,
                        latitude = data_nc$latitude,
                        variable_name = data_nc$variable_name)

    result <- list(result, annual.sd)
  }

  if("monthly" %in% time_val) {
    year_month <- unique(data_nc$dt_iso_dwmy[, .(year, month)])
    monthly.sd <- array(NA, c(length(data_nc$longitude), length(data_nc$latitude), year_month[, .N]))

    for(ym in seq_len(year_month[, .N])) {

      monthly.sd[ , , ym] <- apply(data_nc$value_array[, , data_nc$dt_iso_dwmy[year == year_month[ym, year] &
                                      month == year_month[ym, month], indx]], c(1, 2), sd, na.rm = T)
    }

    monthly.sd <- list(  metric_name = "sd.monthly",
                         value_array = monthly.sd,
                         date_extract = year_month,
                         longitude = data_nc$longitude,
                         latitude = data_nc$latitude,
                         variable_name = data_nc$variable_name)

    result <- list(result, monthly.sd)
  }

  if("weekly" %in% time_val) {
    year_month_week <- unique(data_nc$dt_iso_dwmy[, .(year, month, week)])
    weekly.sd <- array(NA, c(length(data_nc$longitude), length(data_nc$latitude), year_month_week[, .N]))

    for (ymw in seq_len(year_month_week[, .N])) {

      weekly.sd[ , , ymw] <- apply(data_nc$value_array[, , data_nc$dt_iso_dwmy[year == year_month_week[ymw, year] &
                                      month == year_month_week[ymw, month] & week == year_month_week[ymw, week], indx]],
                                      c(1, 2), sd, na.rm = T)
    }

    weekly.sd <-   list( metric_name = "sd.weekly",
                         value_array = weekly.sd,
                         date_extract = year_month_week,
                         longitude = data_nc$longitude,
                         latitude = data_nc$latitude,
                         variable_name = data_nc$variable_name)

    result <- list(result, weekly.sd)
  }

  if("season" %in% time_val) {

    if(is.null(season_month)){cat("You need to define the season_month argument")
                              stop()}

    if(is.null(season_month)){cat("You need to define the season_n argument")
                             stop()}

    year_season <- unique(data_nc$dt_iso_dwmy[month == season_month, .(year)])
    season.sd <- array(NA, c(length(data_nc$longitude), length(data_nc$latitude), year_season[, .N]))
    season_date <- data.table::data.table()
    for (ys in seq_len(year_season[, .N])) {

      start_indx <- min(data_nc$dt_iso_dwmy[year == year_season[ys, year] & month == season_month, indx])

      if(data_nc$dt_iso_dwmy[,.N] < c(start_indx + season_n)){
        season_date <- rbind(season_date,data.table::data.table(data_nc$dt_iso_dwmy[start_indx, date],
                                                      data_nc$dt_iso_dwmy[start_indx, date]+season_n,
                                                      season_n))
        next()
        }

      season_date <- rbind(season_date,data.table::data.table(data_nc$dt_iso_dwmy[start_indx, date],
                                                    data_nc$dt_iso_dwmy[c(start_indx + season_n), date],
                                                    season_n))

      season.sd[ , , ys] <- apply(data_nc$value_array[, , c(start_indx:(start_indx + season_n))],
                                      c(1, 2), sd, na.rm = T)
    }

    names(season_date) <- c("season_start_date", "season_end_date", "season_length")

    season.sd <-   list( metric_name = "sd.season",
                         value_array = season.sd,
                         date_extract = season_date,
                         longitude = data_nc$longitude,
                         latitude = data_nc$latitude,
                         variable_name = data_nc$variable_name)

    result <- list(result, season.sd)
  }

  if("roll_window" %in% time_val) {

    roll.sd <- apply(data_nc$value_array[ , , ], c(1, 2), RcppRoll::roll_sd, n = window_n, na.rm = T)
    roll.sd <- aperm(roll.sd, c(2, 3, 1))

    roll.sd <- list(  metric_name = "sd.rolling_window",
                      value_array = roll.sd,
                      date_extract = data_nc$dt_iso_dwmy[-c(1:(window_n - 1)), .(date)],
                      longitude = data_nc$longitude,
                      latitude = data_nc$latitude,
                      variable_name = data_nc$variable_name)

    result <- list(result, roll.sd)
    }

  return(result)
}

#' get_thepoint Function
#'
#' Function to retrieve values corresponding to geographic points get_thepoint(data_nc$value_array,nc_index[p,])
#' @param x object obtained from the temporal_mean() function containing the array of averaged values
#' @param nc_index spatial indices corresponding to the coordinates of the geographic points
#' @author Reto Schmucki
#' @details This function is internal and used to extract value from the grid with the function point_grid_extract()
#' @export get_thepoint
#'

get_thepoint <- function(x, nc_index){
  value_vect <- x[nc_index$x_index, nc_index$y_index,]
}


#' point_grid_extract_dt
#'
#' This function extract the mean climatic value (annual mean, monthly mean, sliding window mean) for each geographic points
#' @param data_nc object obtained from the temporal_mean(), temporal_sum() or temporal_sd() functions corresponding to the time-period of interest and the level of averaging
#' @param point_coord a data.frame with three column named "site_id", "longitude", and "latitude" where coordinate of the points are in degree decimal (epsg projection 4326 - wgs 84)
#' @param rd number of decimal to keep for coordinates
#' @author Reto Schmucki
#' @import tcltk
#' @import FNN
#' @import data.table
#' @export point_grid_extract_dt
#'

point_grid_extract_dt <- function(data_nc, point_coord, rd=4) {

  ## Get the layer with the widest spatial extent in the data
  na.profile <- apply(data_nc$value_array, 3, function(x) sum(!is.na(x)))
  max_extent <- which(na.profile == max(na.profile))[1]

  lonlat <- expand.grid(data_nc$longitude, data_nc$latitude)
  tmp.vec <- as.vector(data_nc$value_array[, , max_extent])
  tmp.dt01 <- data.table::data.table(cbind(lonlat, tmp.vec))
  names(tmp.dt01) <- c("lon", "lat", data_nc$variable_name)
  tmp.dt_noNA <- tmp.dt01[!is.na(tmp.vec), ]

  # get index of closest point in the climate grid
  nc_index <- data.table::data.table(site_id = NA, gr.longitude = NA, gr.latitude = NA,
                                    x_index = NA, y_index = NA, pt.x_coord = NA, pt.y_coord = NA)

  pb <- tcltk::tkProgressBar(title = "progress bar", min = 0,max = dim(point_coord)[1], width = 300)

  point_coord <- point_coord[, c("site_id", "longitude", "latitude")]

  for (i in seq_along(point_coord$latitude)){
    tcltk::setTkProgressBar(pb, i, label = paste( round(i / dim(point_coord)[1] * 100, 0), "% extracted"))

    nnindex <- FNN::get.knnx(tmp.dt_noNA[, -3], point_coord[i, -1], 1)

    nc_index <- rbind(nc_index, data.table::data.table(site_id = as.character(point_coord$site_id[i]),
                                          gr.longitude = tmp.dt_noNA[nnindex$nn.index, -3]$lon,
                                          gr.latitude = tmp.dt_noNA[nnindex$nn.index, -3]$lat,
                                          x_index = which(data_nc$longitude == tmp.dt_noNA[nnindex$nn.index, -3]$lon),
                                          y_index = which(data_nc$latitude == tmp.dt_noNA[nnindex$nn.index, -3]$lat),
                                          pt.x_coord = point_coord$lon[i], pt.y_coord = point_coord$lat[i]))
  }
  nc_index <- nc_index[-1,]

  close(pb)

  pb <- tcltk::tkProgressBar(title = "progress bar", min = 0,max = length(nc_index$site_id), width = 300)

  result <- data.table::data.table()

  for (p in seq_along(nc_index$site_id)){
    tcltk::setTkProgressBar(pb, p, label = paste( round(p / length(nc_index$site_id) * 100, 0), "% extracted"))
    result1 <- data.table::data.table(variable = data_nc$variable_name,
                                      site_id = nc_index$site_id[p],
                                      date = data_nc$date_extract,
                                      V1 = round(get_thepoint(data_nc$value_array, nc_index[p, ]), rd),
                                      grid_lat = round(nc_index$gr.latitude[p], rd),
                                      grid_lon = round(nc_index$gr.longitude[p], rd),
                                      site_lat = round(point_coord$latitude[p], rd),
                                      site_lon = round(point_coord$longitude[p], rd))
    result <- rbind(result, result1)
  }

  close(pb)

  return(result)
}
