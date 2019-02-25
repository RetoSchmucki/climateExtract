R --vanilla

library(climateExtract)
source('R\\climateExtract_dt.R')

climate_data <- extract_nc_value_dt(2000, 2007)
clim_sum <- temporal_mean_dt(climate_data, time_v=c("season"), 
                            window_n=10, season_month=11, season_n=120)

str(clim_sum)

str(clim_sum[[2]])
data_nc <- clim_sum[[2]]

point_coord <- data.frame(site_id=c("site1","site2","site3","site4","site5"), longitude=c(28.620000,6.401499,4.359062,-3.579906,-2.590392), latitude=c(61.29000,52.73953,52.06530,50.43031,52.02951))

pt_value <- point_grid_extract_dt(clim_sum[[2]], point_coord)
