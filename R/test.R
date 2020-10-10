R --vanilla

install.packages("devtools")
## install the updated version to get the latest climate data ECAD v.18.

devtools::install_github("RetoSchmucki/climateExtract", force = TRUE)

library(climateExtract)
## load new set of functions
source('R\\climateExtract_dt.R')

nc_data <- RNetCDF::open.nc('tg_ens_mean_0.25deg_reg_v18.0e.nc')

nc_data <- ncdf4::nc_open('tg_ens_mean_0.25deg_reg_v18.0e.nc')

## retrived the climate data for the period of interest, use chunks if it' to big
climate_data <- extract_nc_value_dt(1985, 1986, local_file=FALSE, clim_variable="mean temp", statistic="mean", grid_size=0.1)

## extract seasonal mean by using "season" and setting the duration in number of days with season_n
clim_sum <- temporal_mean_dt(climate_data, time_val=c("season"),
                            window_n=10, season_month=5, season_n=120)

## have a look at the opbject retrieve to extract
str(clim_sum)
str(clim_sum[[2]])

## build a set of point for the example
point_coord <- data.frame(site_id=c("site1","site2","site3","site4","site5"),
                          longitude=c(28.620000, 6.401499, 20.359062, -3.579906, -2.590392),
                          latitude=c(61.29000, 52.73953, 40.06530, 50.43031, 52.02951))

## retrieve the values of interest of each point
pt_value <- point_grid_extract_dt(clim_sum[[2]], point_coord)

pt_value
