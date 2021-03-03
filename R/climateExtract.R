#' extract_nc_value
#'
#' Function to extract climate data from NETCDF file produced by the European Climate Assessment & Dataset for a specific time-period and available at https://surfobs.climate.copernicus.eu/dataaccess/access_eobs.php
#' @param first_year a numeric value defining the first year of the time-period to extract, 1950 if NULL, default=NULL
#' @param last_year a numeric value defining the last year of the time-period to extract, 2014 if NULL, default=NULL
#' @param local_file logical if the .nc data are available on your local disc, if FALSE the data will be downloaded, default=TRUE
#' @param file_path string defining the path of the local file (works only if local_file = TRUE), default=NULL
#' @param sml_chunk string defining the time period to be downloaded. Chunk available are "2011-2020", "1995-2010", "1980-1994", "1965-1979", "1950-1964"
#' @param clim_variable string defining the daily climate variable of interest; "mean temp","max temp","min temp","precipitation", default="mean temp"
#' @param statistic string defining the metric to retrieve, "mean" or "spread", where the mean is computed across the 100 members and is provided as the "best-guess" fields.
#' The spread is calculated as the difference between the 5th and 95th percentiles over the ensemble to provide a measure indicate of the 90\% uncertainty range. For more details 
#' see Cornes et al. (2018) and the guidance on how to use ensemble datasets available from http://surfobs.climate.copernicus.eu/userguidance/use_ensembles.php
#' @param grid_size numeric value in degree defining the resolution of the grid, 0.25 (ca. 27 kilometres in latitude) or 0.1 (ca. 11 kilometres in latitude), default=0.25
#' @author Reto Schmucki
#' @details This function ask you to select the .nc file containing the data of interest from your local disc, if local_file is FALSE, data will be downloaded from the ECAD. 
#' If first_year and last_year are not provided, the function extract the full data set. Smaller chunks of about 15 years of the most recent version of the E-OBS dataset can be specified for download 
#' can be specified directly with the argument "sm_chunk" (period available are 2011-2020, 1995-2010, 1980-1994, 1965-1979, 1950-1964).
#' @import ncdf4
#' @import chron
#' @import utils
#' @export extract_nc_value
#'

extract_nc_value <- function(first_year=NULL, last_year=NULL, local_file=TRUE, file_path=NULL, sml_chunk=NULL, clim_variable="mean temp", statistic="mean", grid_size=0.25) {

  if (local_file == TRUE) {
      if(is.null(file_path)) {
          print("select your climate file [.nc]")
          nc.ncdf <- ncdf4::nc_open(file.choose())
      } else {
          nc.ncdf <- ncdf4::nc_open(file_path)
          }
  } else {
  nc.ncdf <- get_nc_online(first_year=first_year, last_year=last_year, sml_chunk=sml_chunk, clim_variable=clim_variable, statistic=statistic, grid_size=grid_size)
  }
  
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

  init_year <- as.numeric(strsplit(unlist(strsplit(gsub('days since ','',day_since),'-',fixed=TRUE)),' ',fixed=TRUE)[[1]][1])
  init_month <- as.numeric(strsplit(unlist(strsplit(gsub('days since ','',day_since),'-',fixed=TRUE)),' ',fixed=TRUE)[[2]][1])
  init_day <- as.numeric(strsplit(unlist(strsplit(gsub('days since ','',day_since),'-',fixed=TRUE)),' ',fixed=TRUE)[[3]][1])

  avg_temp_transect <- data.frame(julianday=day_vals)
  avg_temp_transect$day <- chron::month.day.year(avg_temp_transect$julianday, c(month = init_month, day =init_day, year = init_year))$day
  avg_temp_transect$month <- chron::month.day.year(avg_temp_transect$julianday, c(month = init_month, day =init_day, year = init_year))$month
  avg_temp_transect$year <- chron::month.day.year(avg_temp_transect$julianday, c(month = init_month, day =init_day, year = init_year))$year

  if (is.null(first_year)){ first_year <- min(avg_temp_transect$year)}
  if (is.null(last_year)){ last_year <- max(avg_temp_transect$year)}

  first.month <- head(avg_temp_transect$month[avg_temp_transect$year==first_year],1)
  first.day <- head(avg_temp_transect$day[avg_temp_transect$year==first_year],1)
  last.month <- tail(avg_temp_transect$month[avg_temp_transect$year==last_year],1)
  last.day <- tail(avg_temp_transect$day[avg_temp_transect$year==last_year],1)

  firstday <- avg_temp_transect$julianday[avg_temp_transect$day==first.day&avg_temp_transect$month==first.month&avg_temp_transect$year==first_year]

  lastday <- avg_temp_transect$julianday[avg_temp_transect$day==last.day&avg_temp_transect$month==last.month&avg_temp_transect$year==last_year]

  date_extract <- avg_temp_transect[avg_temp_transect$julianday >= firstday & avg_temp_transect$julianday <= lastday,]
  date_extract <- as.Date(paste(date_extract$day,date_extract$month,date_extract$year,sep="/"), "%d/%m/%Y")

  tmp.array <- ncdf4::ncvar_get(nc.ncdf,nc_var,start=c(1,1,which(day_vals==firstday)),count=c(nlon,nlat,(lastday-firstday)+1))
  tmp.array[tmp.array == fillvalue$value] <- NA

  result <- list(variable_name=nc_varname,value_array=tmp.array,longitude=lon,latitude=lat,date_extract=date_extract)

  return(result)
}

#' get_nc_online
#'
#' Function to retrieve climate data from https://surfobs.climate.copernicus.eu/dataaccess/access_eobs.php
#' @param first_year a numeric value defining the first year of the time-period to extract, 1950 if NULL, default=NULL
#' @param last_year a numeric value defining the last year of the time-period to extract, 2014 if NULL, default=NULL
#' @param sml_chunk string defining the time period to be downloaded. Chunk available are "2011-2020", "1995-2010", "1980-1994", "1965-1979", "1950-1964"
#' @param clim_variable string defining the daily climate variable of interest; "mean temp","max temp","min temp","precipitation", default="mean temp"
#' @param statistic string defining the metric to retrieve, "mean" or "spread", where the mean is computed across the 100 members and is provided as the "best-guess" fields.
#' The spread is calculated as the difference between the 5th and 95th percentiles over the ensemble to provide a measure indicate of the 90\% uncertainty range. For more details 
#' see Cornes et al. (2018) and the guidance on how to use ensemble datasets available from http://surfobs.climate.copernicus.eu/userguidance/use_ensembles.php
#' @param grid_size numeric value in degree defining the resolution of the grid, 0.25 (ca. 27 kilometres in latitude) or 0.1 (ca. 11 kilometres in latitude), default=0.25
#' @author Reto Schmucki
#' @details This function ask you to select the .nc file containing the data of interest from your local disc, if local_file is FALSE, data will be downloaded from the ECAD. 
#' If first_year and last_year are not provided, the function extract the full data set. Smaller chunks of about 15 years of the most recent version of the E-OBS dataset can be specified for download 
#' can be specified directly with the argument "sm_chunk" (period available are 2011-2020, 1995-2010, 1980-1994, 1965-1979, 1950-1964).
#' @import ncdf4
#' @import utils
#' @export get_nc_online
#'
get_nc_online <- function(first_year=first_year, last_year=last_year, sml_chunk=sml_chunk, clim_variable=clim_variable, statistic=statistic, grid_size=grid_size){

  if(is.null(sml_chunk)){

    cat(paste0("Let's try to get the ",clim_variable," from ",first_year," at ",grid_size," degree resolution \n"))

    if (clim_variable == "mean temp") {clim_var <- paste0("tg_ens_", statistic)}
    if (clim_variable == "min temp") {clim_var <- paste0("tn_ens_", statistic)}
    if (clim_variable == "max temp") {clim_var <- paste0("tx_ens_", statistic)}
    if (clim_variable == "precipitation") {clim_var <- paste0("rr_ens_", statistic)}

    if (grid_size == 0.25) {grid_size <- "0.25deg"}
    if (grid_size == 0.1) {grid_size <- "0.1deg"}

    if (first_year >= 2011) {
         year_toget <- "2011-2020_"
         urltoget <- paste0("https://knmi-ecad-assets-prd.s3.amazonaws.com/ensembles/data/Grid_", grid_size, "_reg_ensemble/", clim_var, "_", grid_size, "_reg_", year_toget, "v22.0e.nc")
         dest_file <- paste0(clim_var,"_",grid_size,"_reg_", year_toget, "v22.0e.nc")
     } else {
        urltoget <- paste0("https://knmi-ecad-assets-prd.s3.amazonaws.com/ensembles/data/Grid_", grid_size, "_reg_ensemble/", clim_var, "_", grid_size, "_reg_v22.0e.nc")
        dest_file <- paste0(clim_var,"_",grid_size,"_reg_v22.0e.nc")
     }
  } else {
    if(!sml_chunk %in% c("2011-2020", "1995-2010", "1980-1994", "1965-1979", "1950-1964")){
      stop("sml_chunk must be one of the following period:\n
            \"2011-2020\", \"1995-2010\", \"1980-1994\", \"1965-1979\" or \"1950-1964\"")
    }else{
      year_toget <- paste0(sml_chunk,"_")

      cat(paste0("Let's try to get the ",clim_variable," from ",first_year," at ",grid_size," degree resolution \n"))

    if (clim_variable == "mean temp") {clim_var <- paste0("tg_ens_", statistic)}
    if (clim_variable == "min temp") {clim_var <- paste0("tn_ens_", statistic)}
    if (clim_variable == "max temp") {clim_var <- paste0("tx_ens_", statistic)}
    if (clim_variable == "precipitation") {clim_var <- paste0("rr_ens_", statistic)}

    if (grid_size == 0.25) {grid_size <- "0.25deg"}
    if (grid_size == 0.1) {grid_size <- "0.1deg"}

         year_toget <- "2011-2020_"
         urltoget <- paste0("https://knmi-ecad-assets-prd.s3.amazonaws.com/ensembles/data/Grid_", grid_size, "_reg_ensemble/", clim_var, "_", grid_size, "_reg_", year_toget, "v22.0e.nc")
         dest_file <- paste0(clim_var,"_",grid_size,"_reg_", year_toget, "v22.0e.nc")
    }
  }
    x <- "N"

    if(file.exists(dest_file)){
        x <- readline("The requested climate data already exist, do you want to download them again? (Y/N) \n")
    	}

    if(!file.exists(dest_file) | x %in% c('Y','y','yes')){
       download.file(urltoget, dest_file, mode = "wb")
    }

    cat(paste0("your data (.nc file) is located in ",getwd(),"/", dest_file, "\n"))

    nc.ncdf <- ncdf4::nc_open(dest_file)

  return(nc.ncdf)
}

#' temporal_mean
#'
#' This function compute mean climatic value for specific periods "annual", "monthly", or using a sliding "window" of specific length, from an object obtained from the extrat_nc_value() function
#' @param data_nc object obtained from the extrat_nc_value() function corresponding to the time-period of interest
#' @param time_avg character string defining the level of averaging, "annual", "monthly", "window"
#' @param win_length the number of days defining the span of the sliding window, default set to 30 days
#' @author Reto Schmucki
#' @details This function can be relatively slow if you compute sliding window average over a long period
#' @import zoo
#' @export temporal_mean
#'

temporal_mean <- function(data_nc, time_avg=c("annual","monthly","window"), win_length=30) {

  result <- list(longitude=data_nc$longitude,latitude=data_nc$latitude)

  first_year <- min(unique(as.numeric(format(data_nc$date_extract, "%Y"))))
  last_year <- max(unique(as.numeric(format(data_nc$date_extract, "%Y"))))

  if ("annual" %in% time_avg){

    annual.mean <- array(NA,c(length(data_nc$longitude),length(data_nc$latitude),(last_year-first_year)+1))
    year_list <- c()
    for( y in unique(as.numeric(format(data_nc$date_extract, "%Y")))) {
      annual.mean[,,y-(first_year-1)] <- apply(data_nc$value_array[,,as.numeric(format(data_nc$date_extract, "%Y"))==y],c(1,2),mean,na.rm=T)
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

#' temporal_sum
#'
#' This function compute sum of climatic value for specific periods "annual", "monthly", or using a sliding "window" of specific length, from and object obtained from the extrat_nc_value() function
#' @param data_nc object obtained from the extrat_nc_value() function corresponding to the time-period of interest
#' @param time_sum character string defining the level of summation, "annual", "monthly", "window"
#' @param win_length the number of days defining the span of the sliding window, default set to 30 days
#' @author Reto Schmucki
#' @details This function can be relatively slow if you compute sliding window total over a long period
#' @import zoo
#' @export temporal_sum
#'

temporal_sum <- function(data_nc, time_sum=c("annual","monthly","window"), win_length=30) {

  result <- list(longitude=data_nc$longitude,latitude=data_nc$latitude)

  first_year <- min(unique(as.numeric(format(data_nc$date_extract, "%Y"))))
  last_year <- max(unique(as.numeric(format(data_nc$date_extract, "%Y"))))

  if ("annual" %in% time_sum){

    annual.sum <- array(NA,c(length(data_nc$longitude),length(data_nc$latitude),(last_year-first_year)+1))
    year_list <- c()
    for( y in unique(as.numeric(format(data_nc$date_extract, "%Y")))) {
      annual.sum[,,y-(first_year-1)] <- apply(data_nc$value_array[,,as.numeric(format(data_nc$date_extract, "%Y"))==y],c(1,2),sum,na.rm=F)
      year_list <- c(year_list,y)
    }
    annual.sum <- list(value_array=annual.sum,date_extract=year_list,longitude=data_nc$longitude,latitude=data_nc$latitude,variable_name=data_nc$variable_name)
    result <- c(result,annual.sum)
  }

  if ("monthly" %in% time_sum) {

    year_month <- unique(as.numeric(format(data_nc$date_extract, "%Y%m")))
    monthly.sum <- array(NA,c(length(data_nc$longitude),length(data_nc$latitude),length(year_month)))

    for (ym in year_month) {
      monthly.sum[,,which(year_month==ym)] <- apply(data_nc$value_array[,,as.numeric(format(data_nc$date_extract, "%Y%m"))==ym],c(1,2),sum,na.rm=F)
    }
    monthly.sum <- list(value_array=monthly.sum,date_extract=year_month,year_month=data.frame(year=substr(year_month,1,4),month=substr(year_month,5,6))
                         ,longitude=data_nc$longitude,latitude=data_nc$latitude,variable_name=data_nc$variable_name)
    result <- c(result,monthly.sum)
  }


  if ("window" %in% time_sum) {

    roll.sum <- apply(data_nc$value_array[,,],c(1,2),zoo::rollsum,k=win_length,na.rm=F)
    roll.sum <- aperm(roll.sum, c(2,3,1))
    roll.sum <- list(value_array=roll.sum,date_extract=data_nc$date_extract[-c(1:(win_length-1))],longitude=data_nc$longitude,latitude=data_nc$latitude,variable_name=data_nc$variable_name)
    result <- c(result,roll.sum)
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

get_thepoint <- function(x,nc_index){
  value_vect <- x[nc_index$x_index,nc_index$y_index,]
}


#' point_grid_extract
#'
#' This function extract the mean climatic value (annual mean, monthly mean, sliding window mean) for each geographic points
#' @param data_nc object obtained from the temporal_mean() function corresponding to the time-period of interest and the level of averaging
#' @param point_coord a data.frame with three column named "site_id", "longitude", and "latitude" where coordinate of the points are in degree decimal (epsg projection 4326 - wgs 84)
#' @author Reto Schmucki
#' @import tcltk
#' @import FNN
#' @export point_grid_extract
#'

point_grid_extract <- function(data_nc,point_coord) {

  ## Get the layer with the widest spatial extent in the data
  na.profile <- apply(data_nc$value_array,3,function(x) sum(!is.na(x)))
  max_extent <- which(na.profile==max(na.profile))[1]

  lonlat <- expand.grid(data_nc$longitude,data_nc$latitude)
  tmp.vec <- as.vector(data_nc$value_array[,,max_extent])
  tmp.df01 <- data.frame(cbind(lonlat,tmp.vec))
  names(tmp.df01) <- c("lon","lat",data_nc$variable_name)
  tmp.df_noNA <- tmp.df01[!is.na(tmp.df01[,3]) ,]

  # get index of closest point in the climate grid
  nc_index<-data.frame(site_id=NA,gr.longitude=NA,gr.latitude=NA,x_index=NA,y_index=NA,pt.x_coord=NA,pt.y_coord=NA)

  pb <- tcltk::tkProgressBar(title = "progress bar", min = 0,max = dim(point_coord)[1], width = 300)

  point_coord <- point_coord[,c("site_id","longitude","latitude")]

  for (i in 1:dim(point_coord)[1]){
    tcltk::setTkProgressBar(pb, i, label=paste( round(i/dim(point_coord)[1]*100, 0),"% extracted"))

    nnindex<-FNN::get.knnx(tmp.df_noNA[,-3],point_coord[i,-1],1)

    nc_index <- rbind(nc_index,data.frame(site_id=as.character(point_coord$site_id[i]),gr.longitude=tmp.df_noNA[nnindex$nn.index,-3]$lon,gr.latitude=tmp.df_noNA[nnindex$nn.index,-3]$lat,
                                          x_index=which(data_nc$longitude == tmp.df_noNA[nnindex$nn.index,-3]$lon),y_index=which(data_nc$latitude ==tmp.df_noNA[nnindex$nn.index,-3]$lat),
                                          pt.x_coord=point_coord$lon[i],pt.y_coord=point_coord$lat[i]))
  }
  nc_index <- nc_index[-1,]

  close(pb)

  pb <- tcltk::tkProgressBar(title = "progress bar", min = 0,max = length(nc_index$site_id), width = 300)

  result <- data.frame()

  for (p in 1:length(nc_index$site_id)){
    tcltk::setTkProgressBar(pb, p, label=paste( round(p/length(nc_index$site_id)*100, 0),"% extracted"))
    result1 <- get_thepoint(data_nc$value_array,nc_index[p,])
    result <- rbind(result,result1)
  }

  result <- as.data.frame(t(result))
  names(result) <- nc_index$site_id
  result$date_extract <- data_nc$date_extract
  result <- result[,c(dim(result)[2],c(1:(dim(result)[2]-1)))]

  close(pb)

  return(result)
}
